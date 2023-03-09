(ns mylib.react
  (:require [helix.core :as h])
  (:require-macros [mylib.react]))

(def ^:private react (h/get-react))
(def forward-ref (.-forwardRef react))
(def clone-element (.-cloneElement react))
(def valid-element? (.-isValidElement react))
