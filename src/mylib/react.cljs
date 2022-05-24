(ns mylib.react
  (:require ["react" :refer [forwardRef]]
            [goog.string :as gstring])
  (:require-macros [mylib.react]))

(def forward-ref forwardRef)

(defn format [fmt & args]
  (apply gstring/format fmt args))
