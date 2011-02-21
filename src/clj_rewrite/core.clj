
(ns clj-rewrite.core
  "Term rewriting in clojure"
  (:require [clojure.zip :as zip]))

;; Max number of calls to substitute-node during a call to rewrite
(def *substitute-limit* 100)

;; Indicate if the execution should be verbose
(def *verbose* true)

(defn- substitute-node
  "Match the given rule to the current node in the
   given zipper. If a substitution is made return a modified zipper.
   If no substitition is made, return nil." 
  [zipper rule]
  (let [node (zip/node zipper)]
    (if (coll? node)
      (do
        (set! *substitute-limit* (dec *substitute-limit*))
        (if (< *substitute-limit* 0)
          (throw (Exception. "Exceeded maximum calls to substitute")))
        (when *verbose* (println "considering" node))
        (if-let [res (rule node)]
          (do
            (when *verbose* (println "  ->" res))
            (zip/replace zipper res)))))))

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
  "Apply the given rule to the given zipper moving from leaves
   to root in a left to right order. Returns the root node after any
   substitutions."
  [zipper rule]
  (loop [current (bottom-left zipper)]
    (if-let [new-node (substitute-node current rule)]
      (recur (bottom-left new-node))
      (let [dive (-> current zip/right bottom-left)]
        (if dive
          (recur dive)
          (if-let [rise (zip/up current)]
            (recur rise)
            (zip/root current)))))))

(defn rewrite
  "Rewrites the given datastructure using the given rule"
  [d rule & {:keys [limit verbose]}]
  (binding [*substitute-limit* (or limit *substitute-limit*)
            *verbose* (or verbose *verbose*)]
    (left-most-inner (zip/seq-zip d) rule)))

