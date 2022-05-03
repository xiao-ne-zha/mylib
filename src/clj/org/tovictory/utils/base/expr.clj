(ns org.tovictory.utils.base.expr
  (:require [org.tovictory.utils.base.expr-internal :refer [do-calc operfn if-parser] :as ei]
            [org.tovictory.utils.base.ds.map :refer [rename-keys]]
            [clojure.string :as str]
            [org.tovictory.utils.base.simple-life :refer :all]
            [instaparse.core :as p]
            [clojure.core.cache.wrapped :as cache]))

(defn add-fn
  "增加一个可以在表达式中使用的函数, name-in-expr 在表达式中的名称， 实际调用的函数。
  目前已经支持的操作和函数有：`[floor ceil != 取最大值 取地值 像 min = 日期 * <= / rand like - max 取余数 todate rem 取天值 时差 正则像 取随机数 取最小值 >= sqrt 取商 < abs 取近似值 日差 reglike dayBetween quot hourBetween 取绝对值 round ^ + 取平方根 >]`"
  [name-in-expr f]
  {:pre [(fn? f) (string? name-in-expr) (not (str/blank? name-in-expr))]}
  (swap! operfn assoc name-in-expr f))

(def parser-cache (cache/fifo-cache-factory {}))

(defn calc
  "注意：外部应该判断返回是否是 number? 如果不是，说明返回结果为错误信息，且日志有记录错误信息。
  用data中的属性替换expr表达式中的值，并计算得出最后的结果值. 以下调用二例供参考：
  (calc {:name 100} \"1 *2.0 * round(30.0/150) - :name\") => -100.0
  (calc {\"name\" 100} \"1 *2.0 * round(30.0/150) - name\") => -100.0"
  [params expr opts]
  (let [{:keys [divisor-nil-as divisor-0-as]
         :or {divisor-nil-as ei/*divisor-nil-as*
              divisor-0-as ei/*divisor-0-as*}} opts]
    (when (seq expr)
      (let [ast (cache/lookup-or-miss parser-cache expr if-parser)
            ast (if (p/failure? ast) [nil ast] ast)]
        (binding [ei/*divisor-0-as* divisor-0-as
                  ei/*divisor-nil-as* divisor-nil-as]
          (do-calc params ast))))))

(definterface Calculator
  (^Double calculate [^java.util.Map params ^String expr])
  (^Double calculate [^java.util.Map params ^String expr ^java.util.Map opts]))

(deftype ^{javax.inject.Named {}}
    CalculatorImpl []
  :load-ns true
  Calculator

  (^Double calculate [this ^java.util.Map params ^String expr]
   (.calculate this params expr nil))

  (^Double calculate [this ^java.util.Map params ^String expr ^java.util.Map opts]
   (let [opts (rename-keys opts keyword)
         result (calc params expr opts)]
     (if (p/failure? result)
       (throw (RuntimeException. (str "表达式格式错误：" (with-out-str (instaparse.failure/pprint-failure result)))))
       (if (int? result) (Double/valueOf (str result)) result)))))
