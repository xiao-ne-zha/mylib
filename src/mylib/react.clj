(ns mylib.react
  (:require [helix.core :as h]))

(defmacro defnc [comp-name argv & body]
  (let [comp-ref-name (symbol (str comp-name "-ref"))
        argc (count argv)
        args (first argv)
        ks (get args :keys)
        ks (map keyword ks)
        strs (get args :strs)
        dissoc-ks (concat ks strs)
        as-name (get args :as)
        opts? (map? (first body)) ;; whether an opts map was passed in
        opts (if opts?
               (first body)
               {})
        body (if opts?
               (rest body)
               body)]
    (if (= 0 argc)
      `(h/defnc ~comp-name [] ~@body)
      (if as-name
        (if (= 1 argc)
            ;; argc = 1 ...
          `(h/defnc ~comp-name ~argv
             ~opts
             (let [~as-name (dissoc ~as-name ~@dissoc-ks)]
               ~@body))
            ;; argc = 2 ， 需要转发引用
          `(do
             (h/defnc ~comp-ref-name ~argv
               ~opts
               (let [~as-name (dissoc ~as-name ~@dissoc-ks)]
                 ~@body))
             (def ~comp-name (forward-ref ~comp-ref-name))))
          ;; 没有通过as取剩余参数时
        (if (= 1 argc)
          `(h/defnc ~comp-name ~argv
             ~opts
             ~@body)
            ;; argc = 2 , 需要转发引用
          `(do
             (h/defnc ~comp-ref-name ~argv
               ~opts
               ~@body)
             (def ~comp-name (forward-ref ~comp-ref-name))))))))

(defn- destructure-js [bindings]
  (let [bents (partition 2 bindings)
        pb (fn pb [bvec b v]
             (let [pvec
                   (fn [bvec b val]
                     (let [gvec (gensym "vec__")
                           gseq (gensym "seq__")
                           gfirst (gensym "first__")
                           has-rest (some #{'&} b)]
                       (loop [ret (let [ret (conj bvec gvec val)]
                                    (if has-rest
                                      (conj ret gseq (list `seq gvec))
                                      ret))
                              n 0
                              bs b
                              seen-rest? false]
                         (if (seq bs)
                           (let [firstb (first bs)]
                             (cond
                               (= firstb '&) (recur (pb ret (second bs) gseq)
                                                    n
                                                    (nnext bs)
                                                    true)
                               (= firstb :as) (pb ret (second bs) gvec)
                               :else (if seen-rest?
                                       (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                       (recur (pb (if has-rest
                                                    (conj ret
                                                          gfirst `(first ~gseq)
                                                          gseq `(next ~gseq))
                                                    ret)
                                                  firstb
                                                  (if has-rest
                                                    gfirst
                                                    (list `nth gvec n nil)))
                                              (inc n)
                                              (next bs)
                                              seen-rest?))))
                           ret))))
                   pmap
                   (fn [bvec b v]
                     (let [gmap (gensym "obj__")
                           defaults (:or b)]
                       (loop [ret (-> bvec (conj gmap) (conj v)
                                      ((fn [ret]
                                         (if (:as b)
                                           (conj ret (:as b) gmap)
                                           ret))))
                              bes (let [transforms
                                        (reduce
                                         (fn [transforms mk]
                                           (if (keyword? mk)
                                             (if (#{:keys :syms :strs} mk)
                                               (assoc transforms mk str)
                                               transforms)
                                             transforms))
                                         {}
                                         (keys b))]
                                    (reduce
                                     (fn [bes entry]
                                       (reduce #(assoc %1 %2 ((val entry) %2))
                                               (dissoc bes (key entry))
                                               ((key entry) bes)))
                                     (dissoc b :as :or)
                                     transforms))]
                         (if (seq bes)
                           (let [bb (key (first bes))
                                 bk (val (first bes))
                                 local bb;;(if (instance? clojure.lang.Named bb) (with-meta (symbol nil (name bb)) (meta bb)) bb)
                                 bv #_(if (contains? defaults local)
                                        (list `objget gmap (name bk) (defaults local))
                                        (list `objget gmap (name bk)))
                                 (list (symbol (str ".-" (name bk))) gmap)
                                 #_(list `oops.core/oget gmap (name bk))]
                             (recur (if (ident? bb)
                                      (-> ret (conj local bv))
                                      (pb ret bb bv))
                                    (next bes)))
                           ret))))]
               (cond
                 (symbol? b) (-> bvec (conj b) (conj v))
                 (vector? b) (pvec bvec b v)
                 (map? b) (pmap bvec b v)
                 :else (throw (new Exception (str "Unsupported binding form: " b))))))
        process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
    (if (every? symbol? (map first bents))
      bindings
      (reduce process-entry [] bents))))

(defmacro jslet [bindv & body]
  `(let
       ~(destructure-js bindv)
     ~@body))

(defmacro jsfn [argv & body]
  (let [argc (count argv)
        tmp-args (vec (repeatedly argc gensym))
        bindings (vec (interleave argv tmp-args))]
    `(fn ~tmp-args
       (jslet ~bindings ~@body))))

(defn- gensym? [sym]
  (let [sym (name sym)]
    (re-matches #"(?:obj__|vec__|seq__|first__)\d+" sym)))

(defmacro jsdef [& bindings]
  (let [bindings (vec bindings)
        dest-bindings (destructure-js bindings)
        varnames (->> dest-bindings (partition 2) (map first) (filter (complement gensym?)))]
    `(let ~dest-bindings
       (do
         ~@(map #(list 'def % %) varnames)))))
