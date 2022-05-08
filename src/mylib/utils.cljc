(ns mylib.utils
  (:require [clojure.string :as str]))

(defn- map-to-names [names m dedupe]
  (reduce
   (fn [result [k v]]
     (let [k (name k)]
       (if v
         (conj result k)
         (if dedupe
           (filterv #(not= k %) result)
           result))))
   names
   m))

(defn- inter-classnames [dedupe args]
  (str/join
   " "
   (reduce
    (fn [result arg]
      (cond
        (or (string? arg) (symbol? arg)    (keyword? arg)) (conj result (name arg))
        (or (vector? arg) (list? arg))     (vec (concat result arg))
        (map? arg) (map-to-names result arg dedupe)
        :else result))
    []
    args)))

(defn classnames [& args]
  (inter-classnames false args))
