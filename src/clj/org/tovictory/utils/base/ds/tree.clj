(ns org.tovictory.utils.base.ds.tree
  (:require [clojure.zip :as zip])
  (:import [clojure.lang IPersistentVector IPersistentMap IPersistentList ISeq]))

(defmulti tree-branch? class)
(defmethod tree-branch? :default [_] false)
(defmethod tree-branch? IPersistentVector [v] true)
(defmethod tree-branch? IPersistentMap [m] true)
(defmethod tree-branch? IPersistentList [l] true)
(defmethod tree-branch? ISeq [s] true)
(prefer-method tree-branch? IPersistentList ISeq)

(defmulti tree-children class)
(defmethod tree-children IPersistentVector [v] v)
(defmethod tree-children IPersistentMap [m] (seq m))
(defmethod tree-children IPersistentList [l] l)
(defmethod tree-children ISeq [s] s)
(prefer-method tree-children IPersistentList ISeq)

(defmulti tree-make-node (fn [node children] (class node)))
(defmethod tree-make-node IPersistentVector [v children]
  (vec children))
(defmethod tree-make-node IPersistentMap [m children]
  (apply hash-map (apply concat children)))
(defmethod tree-make-node IPersistentList [_ children]
  children)
(defmethod tree-make-node ISeq [node children]
  (apply list children))
(prefer-method tree-make-node IPersistentList ISeq)

(defn tree-zipper [node]
  (zip/zipper tree-branch? tree-children tree-make-node node))

(defn tree-edit
  "用于查找树种的某个节点，并对节点进行修改.
   matcher -- 单参函数，参数为树的一个节点，返回匹配的结果；
   editor -- 双参函数，第一个参数为matcher返回的结果，第二个参数为当前树节点，返回值为新节点"
  [zipper matcher editor]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next (zip/edit loc (partial editor matcher-result))))
        (recur (zip/next loc))))))

(defn tree-do-loc
  "用于查找树种的某个节点，并可根据改节点的loc调用zip函数进行树的节点增删改操作"
  [zipper matcher do-fn]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if (matcher (zip/node loc))
        (recur (zip/next (do-fn loc)))
        (recur (zip/next loc))))))

(defn tree-replace
  "用于替换某些节点"
  [zipper smap]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [node (get smap (zip/node loc))]
        (recur (zip/next (zip/replace loc node)))
        (recur (zip/next loc))))))
