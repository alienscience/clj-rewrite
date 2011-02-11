
(ns clj-rewrite.dsl
  "Macros for specifing rewrite rules"
  (:use clj-rewrite.rule)
  (:use clojure.walk))


(defn- split-symbol
  "Splits a symbol into name and last character strings.
   Returns [name last-char] if last character is + or *
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


(declare convert-substitution)

(defn- convert-sub-element
  "Converts an element of a substitution into functional form"
  [e]
  (cond
    (symbol? e)    `(sub ~(keyword e))
    (seq? e)       (convert-substitution e)
    :else          e))

(defn- convert-substitution
  "Converts a substitution specification into a function
   definition"
  [sub-spec]
  (cond
    ;;---
    (list? sub-spec)
    (let [first-element (first sub-spec)
          arg1 (if (symbol? first-element)
                 (list 'quote first-element)
                 (convert-sub-element first-element))
          args (map convert-sub-element (next sub-spec))]
      `(build-list ~arg1 ~@args))
    ;;---
    :else
    `(return-element ~(convert-sub-element sub-spec))))

(defn parse-rewrite
  "Returns a sequence of rewrite rules in a functional form
   from the spec of a def-rewrite"
  [raw]
  (if-not (empty? raw)
    (let [[match-spec when when-spec sub-spec] (take 4 raw)]
      (cond
        ;---
        (and (= :when when))
        (let [[pattern binds] (convert-match match-spec)
              when-fn (build-when-fn when-spec binds)
              subs-fn (convert-substitution sub-spec)]
          (cons `(rule ~pattern ~when-fn ~subs-fn)
                (lazy-seq (parse-rewrite (nthnext raw 4)))))
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
          (str "Malformed rule beginning with " match-spec)))))))

(defmacro def-rewrite
  "Defines a set of rewrite rules"
  [identifier & body]
  (let [h (first body)
        doc-string (if (string? h) h)]
    (if doc-string
      (let [func-form (parse-rewrite (next body))]
        `(def ~(with-meta identifier {:doc doc-string})
              (combine ~@func-form)))
      (let [func-form (parse-rewrite body)]
        `(def ~identifier
              (combine ~@func-form))))))
