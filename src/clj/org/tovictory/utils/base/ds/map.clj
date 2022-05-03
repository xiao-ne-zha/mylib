(ns org.tovictory.utils.base.ds.map)

(defn rename-keys
  "使用(f k)对m中的所有key进行重命名"
  [m f]
  (when m
    (into {} (map (fn [[k v]] [(f k) v]) m))))
