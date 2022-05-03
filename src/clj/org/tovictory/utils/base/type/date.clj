(ns org.tovictory.utils.base.type.date
  (:require [org.tovictory.utils.base.simple-life :refer :all])
  (:import [java.text SimpleDateFormat]
           [java.util Date]))

(def- ^SimpleDateFormat default-fmt (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

(defn ^Date to-date
  ([^String string] (when (not-empty string) (.parse default-fmt string)))
  ([^String string ^String fmt]
   (when (and (not-empty string) (not-empty fmt))
     (.parse (SimpleDateFormat. fmt) string))))

(defn ^String to-str
  ([^Date dt] (when dt (.format default-fmt dt)))
  ([^Date dt ^String fmt]
   (when (and dt (not-empty fmt))
     (.format (SimpleDateFormat. fmt) dt))))
