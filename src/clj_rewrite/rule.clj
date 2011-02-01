
(ns clj-rewrite.rule
  "Rule building for clj-rewrite"
  (:use clojure.walk))

(def binds* nil)

(defn- mapcat-pipe
  "Takes a sequence of functions [f1 f2 .. fm] and returns a function
   (fn [s]
     (mapcat fn (... (mapcat f2 (f1 s)))))"
  [fns]
  (reduce (fn [f1 f2]
            (fn [s] (mapcat f2 (f1 s))))
          fns))

(defn- match-repeat
  "Validates a repeated match"
  [k {:keys [match todo] :as state}]
  (let [val (match k)]
    (loop [rep (if (vector? val) val [val])
           to-check todo]
      (if (empty? rep)
        [{:match match :todo to-check}]
        (if (= (first rep) (first to-check))
          (recur (next rep) (next to-check)))))))

(defn- extend-match
  "Extends a match that is taking place"
  [k {:keys [match todo] :as state}]
  (if-let [current (first todo)]
    (let [new-state {:match (update-in match [k] conj current)
                     :todo (next todo)}]
      (cons new-state
            (lazy-seq (extend-match k new-state))))))

(defn- one-or-more-match
  "Returns a sequence of matches if at least one match"
  [k {:keys [match todo] :as state}]
  (if (contains? match k)
    (match-repeat k state)
    (extend-match k (update-in state [:match] assoc k []))))

(defn- wildcard-match
  "Returns a sequence of wildcard matches"
  [k {:keys [match todo] :as state}]
  (if (contains? match k)
    (match-repeat k state)
    (let [new-state (update-in state [:match] assoc k [])]
      (cons new-state
       (extend-match k new-state)))))

(defn- single-match
  "Returns a sequence containing a single match if available"
  [k {:keys [match todo] :as state}]
  (if (contains? match k)
    (match-repeat k state)
    (if-let [current (first todo)]
      [{:match (assoc match k current)
        :todo (next todo)}])))

(defn- optional-match
  "Returns a sequence containing matches for an optional symbol"
  [k {:keys [match] :as state}]
  (if (contains? match k)
    (match-repeat k state)
    (cons state (single-match k state))))

(defn- match-equals
  "Only match against the given item"
  [item {:keys [match todo]}]
  (if-let [current (first todo)]
    (if (= item current)
      [{:match match :todo (next todo)}])))

(defn- match-end
  "Match against end of sequence"
  [{:keys [todo] :as state}]
  (if (empty? todo)
    [state]))

(declare make-match-generator)

(defn- make-symbol-matcher
  "Converts symbol in a pattern into a function that matches against
   that symbol"
  [sym]
  (cond
    (fn? sym)       sym
    (vector? sym)   (make-match-generator sym)
    :else           (partial match-equals sym)))

(defn- make-match-generator
  "Converts a pattern into a function that returns a sequence of
   matches to that pattern."
  [pattern]
  (let [fns (map make-symbol-matcher (conj pattern match-end))]
    (mapcat-pipe fns)))

(defn- make-matcher
  "Returns a function that will match once against the given pattern
   and when predicate"
  [pattern when-fn]
  (let [matcher (make-match-generator pattern)]
    (if when-fn
      (fn [d]
        (first
         (filter when-fn
                 (map :match (matcher {:match {} :todo d})))))
      (fn [d]
        (first (map :match (matcher {:match {} :todo d})))))))

(defn match*
  "Match zero or more items and identify the matches with key k"
  [k]
  (partial wildcard-match k))

(defn match+
  "Match 1 or more items and identify the matches with key k"
  [k]
  (partial one-or-more-match k))

(defn match
  "Match a single item and identify the match with key k"
  [k]
  (partial single-match k))

(defn match?
  "Match an optional item and identify the match with key k"
  [k]
  (partial optional-match k))

(defn sub
  "Return the value for the match with the given key"
  [k]
  (fn [m] 
    (let [ret (m k)]
      (if (vector? ret) ret [ret]))))

(defn build-list
  "Build a list of results with the given contents"
  [& contents]
  (fn [m]
    (apply list 
           (mapcat (fn [x]
                     (cond
                       (fn? x)     (x m)
                       (vector? x) x
                       :else       [x])) 
                   contents))))

(defn- split-symbol
  "Splits a symbol into name and last character strings.
   Returns [name last-char] -sif last character is + or *
   or returns [name nil]."
  [s]
  (let [as-str (str s)
        end (.length as-str)
        last-char (.substring as-str (dec end) end)]
    (if (contains? #{"+" "*"} last-char)
      [(.substring as-str 0 (dec end)) last-char]
      [as-str nil])))

(defn- convert-match-symbol
  "Converts a symbol specifying a match into a functional
   form"
  [s]
  (let [[name last-char] (split-symbol s)
        k (keyword name)]
    (set! binds* (conj binds* k))
    (condp = last-char
      "+"   `(match+ ~k)
      "*"   `(match* ~k)
      `(match ~k))))


(declare convert-match-seq)

(defn- convert-match-element
  "Converts an element used for matching into a functional form"
  [s]
  (cond
    (symbol? s)                      (convert-match-symbol s)
    (and (seq? s) 
         (not= 'quote (first s)))    (convert-match-seq s)
    :else                            s))

(defn- convert-match-seq
  "Converts a sequence used for matching into a functional form"
  [s]
  (walk convert-match-element vec s))

(defn- convert-match
  "Converts a datastructure used to specify matches into
   a functional form. Returns the new datastructure and
   a vector of keys that will be set by matching." 
  [ds]
  (binding [binds* []]
    (let [pattern (apply vector
                         (list 'quote (first ds))
                         (convert-match-seq (next ds)))]
      [pattern binds*])))

(defn rule
  "Return a function that implements the given substitution rule."
  ([pattern subs-fn] (rule pattern nil subs-fn))
  ([pattern when-fn subs-fn]
     (let [match-fn (make-matcher pattern when-fn)]
       (fn [d]
         (if-let [m (match-fn d)]
           (subs-fn m))))))

(defn- destructure-when
  "Destructures a map from a match to provide bindings for a
   when statement"
  [binds]
  (let [symbols (map #(-> % name symbol) binds)]
    `{:keys [~@symbols]}))

(defn- build-when-fn
  "Converts a when specification into a function definition"
  [spec binds]
  (let [destructuring (destructure-when binds)]
    `(fn [m#]
       (let [~destructuring m#]
         ~spec))))

(defn- get-builder
  "Returns symbol identifying the function to build the 
   given substitution specification"
  [sub-spec head]
  (cond
    ;;---
    (list? sub-spec)    
    ['build-list 
     (if (symbol? head) 
       '(quote head) 
       (convert-sub-element head)))]
    ;;---
    :else
    (throw
     (.Exception 
      (str "Don't know how to build a subtitution from "
           (type sub-spec))))))

(defn- convert-substitution
  "Converts a substitution specification into a function
   definition"
  [sub-spec]
  (let [builder (get-builder sub-spec (first sub-spec))
        s (map convert-sub-element (next sub-spec))])
  `(~@builder ~@s))


(defn parse-rewrite
  "Returns a sequence of rewrite rules in a functional form
   from the spec of a def-rewrite"
  [& raw]
  (let [[match-spec when when-spec sub-spec] (take 5 raw)]
    (cond
      ;---
      (and (= :when when))
      (let [[pattern binds] (convert-match match-spec)
            when-fn (build-when-fn when-spec binds)
            subs-fn (convert-substitution sub-spec)]
        (cons `(rule ~pattern ~when-fn ~subs-fn)
              (lazy-seq (parse-rewrite (nthnext raw 5)))))
      ;---
      (not (nil? when))
      (let [[pattern] (convert-match match-spec)
            subs-fn (convert-substitution when)]
        (cons `(rule ~pattern ~subs-fn)
              (lazy-seq (parse-rewrite (nthnext raw 2)))))
      ;---
      :else
      (throw 
       (.Exception 
        (str "Malformed rule beginning with " match-spec))))))

;; TODO: support optional documentation string
(defmacro def-rewrite
  "Defines a set of rewrite rules"
  [& raw]
  )
