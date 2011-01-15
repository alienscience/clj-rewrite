
(ns clj-rewrite.core
  "Term rewriting in clojure"
  (:require [clojure.zip :as zip]))

;; Max number of calls to substitute-node during a call to rewrite
(def *substitute-limit* 100)

;; Indicate if the execution should be verbose
(def *verbose* true)

(defn- substitute-node
  "Match the given sequence of substitutions to the current node in the
   given zipper. If a substitution matches, make the substitution and
   return a modified zipper. If no substititions match, return nil." 
  [zipper s]
  (set! *substitute-limit* (dec *substitute-limit*))
  (if (< *substitute-limit* 0)
    (throw (Exception. "Exceeded maximum calls to substitute")))
  (let [node (zip/node zipper)]
    (when *verbose* (println "Considering" node))
    (loop [i 0]
      (if (< i (count s))
        (let [substitute (nth s i)
              res (substitute node)]
          (if res
            (zip/replace zipper res)
            (recur (inc i))))))))

(defn- bottom-left
  "Move to the bottom left of the given zipper, returns nil if the zipper
   is nil"
  [z]
  (if z
    (loop [current z
           down (zip/down z)]
      (if (nil? down)
        current
        (recur down (zip/down down))))))

(defn- left-most-inner
  "Apply the given substitutions to the given zipper moving from leaves
   to root in a left to right order. Returns the root node after any
   substitutions."
  [zipper s]
  (loop [current (bottom-left zipper)]
    (if-let [new-node (substitute-node current s)]
      (recur new-node)
      (let [dive (-> current zip/right bottom-left)]
        (if dive
          (recur dive)
          (if-let [rise (zip/up current)]
            (recur rise)
            (zip/root current)))))))

(defn rewrite
  "Rewrites the given datastructure using the given rules"
  [d rules & {:keys [limit verbose]}]
  (binding [*substitute-limit* (or limit *substitute-limit*)
            *verbose* (or verbose *verbose*)]
    (left-most-inner (zip/seq-zip d) rules)))

