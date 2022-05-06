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
     (defmacro ^:private log-impl [level level-name body]
       `(when (>= ~level @log-level)
          (js/console.log (get-now) ~(name level-name) ~@body)))
     (defmacro debug [& body]
       `(log-impl 0 DEBUG ~body))
     (defmacro info [& body]
       `(log-impl 1 INFO ~body))
     (defmacro warn [& body]
       `(log-impl 2 WARN ~body))
     (defmacro error [& body]
       `(log-impl 3 ERROR ~body))))
