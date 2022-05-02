(ns mylib.reframe
  #?(:cljs (:require [lambdaisland.glogi :as log]
                     ["recoil" :as rc]))
  #?(:cljs (:require-macros [mylib.reframe])))

#?(:cljs
   (do
     (def raw-use-recoil-value rc/useRecoilValue)
     (def raw-selector rc/selector)
     (def raw-selector-familiy rc/selectorFamily)
     (def raw-use-set-recoil-state rc/useSetRecoilState)
     (def RecoilRoot rc/RecoilRoot)
     (def raw-atom rc/atom)
     (def ->js clj->js)))

(defn get-default-reducer-name [state-name]
  (symbol (str state-name "-reducer")))

#?(:clj
   (do
     (defmacro defsub [query-id [getfn-sym & args] body]
       (let [key-name (name query-id)
             view-name (symbol key-name)]
         (if (= 0 (count args))
           `(def ~view-name
              (raw-selector
               (->js {:key ~key-name
                      :get (fn [state#]
                             (let [~getfn-sym (.-get state#)] ~body))})))
           `(def ~view-name
              (raw-selector-familiy
               (->js {:key ~key-name
                      :get (fn ~(vec args)
                             (fn [state#]
                               (let [~getfn-sym (.-get state#)] ~body)))}))))))
     (defmacro defsub-by-keys [query-id [state & keys-vec]]
       (let [keys-vec (vec keys-vec)
             getsym (symbol "get")]
         `(defsub ~query-id [~getsym] (get-in (~getsym ~state) ~keys-vec))))

     (defmacro use-dispatch [state]
       `(raw-use-set-recoil-state ~(get-default-reducer-name state)))
     (defmacro use-sub
       ([query-id]
        `(raw-use-recoil-value
          ~(symbol (str query-id))))
       ([query-id & args]
        `(raw-use-recoil-value (~(symbol (str query-id)) ~@args))))))

#?(:cljs
   (do
     (defmulti event-fx (fn [_ action] (:type action)) :default nil)
     (defmethod event-fx nil
       [_ action]
       (throw (js/Error. (str "Cannot perform action " action))))

     (defmulti effect-fx (fn [_ {:keys [type]}] type) :default nil)
     (defmethod effect-fx nil
       [_ effect]
       (throw (js/Error. (str "Cannot perform effect handler :" effect))))

     (defn on-effect [effect-type]
       (fn [atom-effect]
         (let [on-set (.-onSet atom-effect)
               set-self (.-setSelf atom-effect)
               dispatch (fn [action] (set-self #(event-fx % action)))]
           (on-set
            (fn [{:keys [effects]}]
              (let [to-execute (filter #(= effect-type (:type %)) effects)]
                (when-not (empty? to-execute)
                  (set-self
                   (fn [{:keys [db effects]}]
                     {:db db
                      :effects (vec (filter #(not= effect-type (:type %)) effects))}))
                  (doseq [e to-execute]
                    (log/debug "do effect:" e)
                    (effect-fx dispatch e)))))))))
     (defn init-app-state [kname default effect-names]
       (raw-atom #js {:key kname
                      :default default
                      :effects (->> effect-names (map on-effect) clj->js)}))
     (defn init-app-state-reducer [kname app-state]
       (raw-selector #js {:key kname
                          :set (fn [state action]
                                 (let [set (.-set state)]
                                   (set app-state #(merge % (event-fx % action)))))
                          :get (fn [state]
                                 (let [get (.-get state)]
                                   (get app-state)))}))))

#?(:clj
   (do
     (defmacro defstate
       [state-name init-value effect-names]
       (let [kname (name state-name)
             reducer-name (get-default-reducer-name state-name)]
         `(do
            (def ~state-name
              (init-app-state ~kname ~init-value ~effect-names))
            (def ~reducer-name
              (init-app-state-reducer ~(name reducer-name) ~state-name)))))
     (defmacro reg-event-fx [event-type args body]
       `(defmethod event-fx ~event-type ~args ~body))
     (defmacro reg-effect-fx [effect-type [dispatch event] body]
       `(defmethod effect-fx ~effect-type ~[dispatch event] ~body))))
