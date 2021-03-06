
(ns clj-rewrite.rule
  "Rule building for clj-rewrite")

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

(defn- match-subseq
  "Match against a subsequence"
  [match-fn {:keys [match todo]}]
  (if-let [current (first todo)]
    (if (coll? current)
      (let [n (next todo)]
        (for [new-state (match-fn {:match match :todo current})
              :let [new-match (:match new-state)]]
          {:match new-match :todo n})))))

(declare make-match-generator)

(defn- make-element-matcher
  "Converts an element in a pattern into a function that matches against
   that element"
  [el]
  (cond
    (fn? el)        el
    (vector? el)    (partial match-subseq (make-match-generator el))
    :else           (partial match-equals el)))

(defn- make-match-generator
  "Converts a pattern into a function that returns a sequence of
   matches to that pattern."
  [pattern]
  (let [fns (map make-element-matcher (conj pattern match-end))]
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
    (m k)))

(defn mapcat-content
  "Return the given content in a form suitable for putting into a mapcat 
   for the given match."
  [m c]
  (cond
    (fn? c)      (let [ret (c m)]
                   (cond
                     (vector? ret)    ret
                     :else            [ret]))
    (vector? c)  c
    :else        [c]))

(defn build-list
  "Build a list of results with the given contents"
  [& contents]
  (fn [m]
    (apply list 
           (mapcat (partial mapcat-content m) contents))))

(defn each
  "Runs the given function with each value in the match (with key ks)
   removed."
  [k ks f]
  (fn [m]
    (let [vals (m ks)]
      (vec
       (for [i (range (count vals))
             :let [kval (nth vals i)
                   others (vec (concat (subvec vals 0 i)
                                       (subvec vals (inc i))))]]
         (f (merge m {k kval ks others})))))))


(defn return-element
  "Return a single element"
  [e]
  (fn [m]
    (cond
      (fn? e)     (let [res (e m)]
                    (if (coll? res)
                      (if (> (count res) 1)
                        res
                        (first res))
                      res))
      :else       e)))

(defn rule
  "Return a function that implements the given substitution rule."
  ([pattern subs-fn] (rule pattern nil subs-fn))
  ([pattern when-fn subs-fn]
     (let [match-fn (make-matcher pattern when-fn)]
       (fn [d]
         (if-let [m (match-fn d)]
           (subs-fn m))))))

(defn combine
  "Combine a sequence of rules into a single rule"
  [& rules]
  (fn [x]
    (first (remove nil?
                   (map #(% x) rules)))))


(defn edit-rules
  "Return a function that implements the given edit rules"
  [& rules]
  (let [editor (lazy-pipe rules)]
    (fn [d]
      (let [{:keys edit? s} (editor {:edit? false :s (seq d)})]
        (if edit?
          (apply list s))))))
