(ns org.tovictory.utils.base.ds.seq
  (:require [org.tovictory.utils.base.ds.map :as m]))

(defn rename-keys
  "对seq中的所有map元素使用(f k)进行key转换"
  [s f]
  (when s
    (into (empty s)
          (map #(if (instance? clojure.lang.IPersistentMap %) (m/rename-keys % f) %) s))))
