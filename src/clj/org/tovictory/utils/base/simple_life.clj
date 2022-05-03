(ns org.tovictory.utils.base.simple-life
  (:require [clojure.tools.logging :as log]
            [potemkin :refer [import-vars]]
            [org.tovictory.utils.base.imports]))

(import-vars [org.tovictory.utils.base.imports def-proxied import-as])

(defmacro defmacro-
  "定义私有宏，用法如defmacro"
  [name & decls]
  (list* `defmacro (vary-meta name assoc :private true) decls))

(defmacro def-
  "定义私有变量"
  [name & decls]
  (list* `def (vary-meta name assoc :private true) decls))

(defmacro with-log-time
  "返回对形式的求值，但是在日志中以debug级别输出形式执行的时间，
   多个形式的时候，将给出各个形式的耗时和总耗时, 并返回最后一个形式的值"
  ([expr]
   `(let [start# (. System (nanoTime))
          ret# ~expr]
      (log/debug '~expr "elapsed time:" (/ (double (- (. System (nanoTime)) start#)) 1000000.0) "ms")
      ret#))
  ([e1 & body]
   (let [body (cons e1 body)]
     `(let [start# (. System (nanoTime))
            ret# (do ~@(map #(list `with-log-time %) body))]
        (log/debug '~body "elapsed time:" (/ (double (- (. System (nanoTime)) start#)) 1000000.0) "ms")
        ret#))))

(defn- format-4-log
  "将多个参数转换为 (x=xv, y=yv, z=zv)的list形式"
  [& body]
  (drop 1 (interleave (repeat \,) (map #(str % \=) body) body)))

(defmacro log-value
  "以debug级别输出各个参数的值，使用形式： (log-value x y z)
   日志输出为[ x = xv, y= yv, z = zv]"
  [& body]
  (list* `log/debug (apply format-4-log body)))

(defmacro log-value-with-description
  "以debug级别输出各个参数的值，第一个参数为描述字符串"
  [desc & body]
  (list* `log/debug desc (apply format-4-log body)))

(defmacro with-log
  "返回对形式的求值，但是在日志中以debug级别输出形式输出表达式和求值结果。
  多个形式的时候，将打印各形式的日志, 并返回最后一个形式的值"
  ([expr]
   `(let [ret# ~expr]
      (log/debug '~expr "=" ret#)
      ret#))
  ([e1 & body]
   (let [body (cons e1 body)]
     `(do ~@(map #(list `with-log %) body)))))

(defmacro time-> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       `(time (~(first form) ~x ~@(next form)))
                       `(time (~form ~x)))]
        (recur threaded (next forms)))
      `(time ~x))))
