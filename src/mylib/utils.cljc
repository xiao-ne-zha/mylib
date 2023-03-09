(ns mylib.utils
  (:require [clojure.string :as str]
            [goog.string :as gstring]
            ["tailwind-merge" :refer [twMerge]]))

(defn- map-to-names [m]
  (reduce
   (fn [result [k v]]
     (let [k (name k)]
       (if v
         (conj result k)
         result)))
   []
   m))

(defn classnames [& args]
  (apply twMerge
   (reduce
    (fn [result arg]
      (cond
        (or (string? arg) (symbol? arg) (keyword? arg))
        (conj result (name arg))

        (or (vector? arg) (list? arg))
        (conj result (apply classnames arg))

        (map? arg)
        (vec (concat result (map-to-names arg)))

        :else result))
    []
    args)))

(defn format [fmt & args]
  (apply gstring/format fmt args))
