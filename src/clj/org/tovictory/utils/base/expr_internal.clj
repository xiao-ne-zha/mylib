(ns org.tovictory.utils.base.expr-internal
  (:require [instaparse.core :as p]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [org.tovictory.utils.base.simple-life :refer :all]
            [clojure.tools.logging :as log])
  (:import [java.time LocalDateTime ZoneId LocalDate]
           [java.time.format DateTimeFormatter]
           [java.time.temporal ChronoUnit]))
(def ^:dynamic *nil-as* nil)
(def ^:dynamic *divisor-nil-as* nil)
(def ^:dynamic *divisor-0-as* 0)

(def-proxied (Math pow round abs ceil floor sqrt))
(def- CH-LOCAL-DATE-TIME (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))
(def- TIME-SUFFIX " 00:00:00")

(p/defparser if-parser (slurp (io/resource "cal_expr.bnf")) :auto-whitespace :standard)

(defn- to-date [^String d]
  (let [d (if (= 10 (count d)) (str d TIME-SUFFIX) d)
        fmt (if (str/includes? d "T") DateTimeFormatter/ISO_LOCAL_DATE_TIME CH-LOCAL-DATE-TIME)]
    (LocalDateTime/parse d fmt)))

(defn- day-between [l r]
  (let [l (if (instance? java.util.Date l) (LocalDateTime/ofInstant l (ZoneId/systemDefault)) l)]
    (.between ChronoUnit/DAYS l r)))

(defn- hour-between [l r]
  (let [l (if (instance? java.util.Date l) (LocalDateTime/ofInstant l (ZoneId/systemDefault)) l)]
    (.between ChronoUnit/HOURS l r)))

(defn- like [l r]
  (when (seq r)
    (re-matches (re-pattern (str/replace r "*" ".*")) (str l))))

(defn- reglike [l regex]
  (when (seq regex)
    (re-matches (re-pattern regex) (str l))))

(defn- gt [l r]
  (if (number? l)
    (> l r)
    (> (.compareTo l r) 0)))

(defn- ge [l r]
  (if (number? l)
    (>= l r)
    (>= (.compareTo l r) 0)))

(defn- lt [l r]
  (if (number? l)
    (< l r)
    (< (.compareTo l r) 0)))

(defn- le [l r]
  (if (number? l)
    (<= l r)
    (<= (.compareTo l r) 0)))

(def-  default-rl-op-fn {">=" ge ">" gt "<=" le "<" lt "!=" not= "=" =
                         "like" like "像" like
                         "reglike" reglike "正则像" reglike})

(def- default-math-op-fn {"+" + "-" - "*" * "/" /
                          "^" pow
                          "round" round "取近似值" round
                          "abs" abs "取绝对值" abs
                          "ceil" ceil "取天值" ceil
                          "floor" floor "取地值" floor
                          "max" max "取最大值" max
                          "min" min "取最小值" min
                          "rand" rand "取随机数" rand
                          "sqrt" sqrt "取平方根" sqrt
                          "rem" rem "取余数" rem
                          "quot" quot "取商" quot
                          "todate" to-date "日期" to-date
                          "dayBetween" day-between "日差" day-between
                          "hourBetween" hour-between "时差" hour-between})

(def ^:dynamic ^:no-doc operfn
  (atom (merge default-math-op-fn default-rl-op-fn)))

(defmulti do-calc (fn [data ast] (first ast)) :default nil)

(defmethod do-calc nil [_ [_ err]]
  (log/error "表达式语法错误，错误信息为：" err)
  err)

(defn- do-expr [data [_ operand1 operator operand2 & rs]]
  (loop [result (do-calc data operand1), operator operator, operand2 operand2, rs rs]
    (if operator
      (let [result ((@operfn operator) result (do-calc data operand2))
            result (if (ratio? result) (double result) result)]
        (recur result (first rs) (second rs) (drop 2 rs)))
      result)))
(defmethod do-calc :cc-expr [data ast]
  (do-expr data ast))

(defmethod do-calc :md-expr [data [_ operand1 operator operand2 & rs]]
  (loop [result (do-calc data operand1), operator operator, operand2 operand2, rs rs]
    (if operator
      (if (= "/" operator)
        (let [divisor (do-calc data operand2)
              divisor (if (nil? divisor) *divisor-nil-as* divisor)
              divisor (if (zero? divisor) *divisor-0-as* divisor)
              result (/ result divisor)
              result (if (ratio? result) (double result) result)]
          (recur result (first rs) (second rs) (drop 2 rs)))
        (let [result ((@operfn operator) result (do-calc data operand2))
              result (if (ratio? result) (double result) result)]
          (recur result (first rs) (second rs) (drop 2 rs))))
      result)))

(defmethod do-calc :atom [data [_ afe]]
  (if (string? afe)
    (edn/read-string afe)
    (do-calc data afe)))

(defmethod do-calc :fun_expr [data [_ [_ f] & es]]
  (let [f (@operfn f)]
    (apply f (map #(do-calc data %) es))))

(defmethod do-calc :attr [data [_ attr]]
  (let [attr (if (str/starts-with? attr ":") (->> (drop 1 attr) (apply str) keyword) attr)]
    (get data attr)))

(defmethod do-calc :pow-expr [data ast]
  (do-expr data ast))

(defmethod do-calc :logic-expr [data [_ expr & rs]]
  (loop [result (do-calc data expr), rs rs]
    (if (or result (empty? rs))
      result
      (recur (do-calc data (first rs)) (rest rs)))))

(defmethod do-calc :and-expr [data [_ expr & rs]]
  (loop [result (do-calc data expr), rs rs]
    (if (and result (not-empty rs))
      (recur (do-calc data (first rs)) (rest rs))
      result)))

(defmethod do-calc :le-expr [data [_ expr1 rl-op expr2]]
  (if rl-op
    ((@operfn rl-op) (do-calc data expr1) (do-calc data expr2))
    (if (string? expr1) (edn/read-string expr1)
        (do-calc data expr1))))

(defmethod do-calc :if-expr [data [_ logic-expr then-expr else-expr]]
  (if then-expr
    (if (do-calc data logic-expr)
      (do-calc data then-expr)
      (when else-expr
        (do-calc data else-expr)))
    (do-calc data logic-expr)))

(defmethod do-calc :expr [data [_ expr]]
  (do-calc data expr))
