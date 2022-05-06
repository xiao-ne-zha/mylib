(ns mylib.log
  #?(:cljs (:require ["moment" :as moment]))
  #?(:cljs (:require-macros [contacts.log])))

#?(:cljs
   (do
     (defn get-now []
       (.format (moment) "YYYY-MM-DD hh:mm:ss.SSS"))
     (def ^:private log-level (atom 1))
     (def ^:private log-level-map {:debug 0, :info 1, :warn 2, :error 3})
     (defn set-log-level [level]
       (reset! log-level (get log-level-map level 1)))))

#?(:clj
   (do
     (defmacro debug [& body]
       `(when (>= 0 @log-level)
          (js/console.log (get-now) "DEBUG" ~@body)))
     (defmacro info [& body]
       `(when (>= 1 @log-level)
          (js/console.log (get-now) "INFO" ~@body)))
     (defmacro warn [& body]
       `(when (>= 2 @log-level)
          (js/console.log (get-now) "WARN" ~@body)))
     (defmacro error [& body]
       `(when (>= 3 @log-level)
          (js/console.log (get-now) "ERROR" ~@body)))))
