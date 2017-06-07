(ns bespoke-primes.regex-test
  (:require [bespoke-primes.regex :as regex]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer [are deftest is]]
            [clojure.set :as sets]
            [com.gfredericks.test.chuck.generators :as gen']))

(defn nfa-matches?
  [{:keys [start accept transitions]} s]
  (loop [states #{start}
         chars (map str s)]
    (if-let [[c & more] (seq chars)]
      (let [next-states (->> states
                             (map (fn [state]
                                    (some->> (transitions state)
                                             (filter #((key %) c))
                                             (first)
                                             (val))))
                             (apply sets/union))]
        (if (empty? next-states)
          false
          (recur next-states more)))
      (not (empty? (sets/intersection accept states))))))

(deftest dfa-matching
  (are [regex matching not-matching]
      (let [dfa (regex/compile regex 10)]
        (and (every? #(nfa-matches? dfa %) matching)
             (not-any? #(nfa-matches? dfa %) not-matching)))
    "7123" ["7123"] ["712" "123" "" "71233" "77123"]
    "7|||1" ["7" "1"] ["77" "71"]
    "" [] ["1"]))

(deftest match-enumeration
  (is (empty? (-> "" (regex/compile 10) regex/all-nonempty-matches)))
  (are [regex base matches-prefix]
      (let [dfa (regex/compile regex base)]
        (is (= matches-prefix
               (take (count matches-prefix) (regex/all-nonempty-matches dfa)))))
      "(00|111)*" 10
      ["111" "11100" "111111" "1110000" "11100111" "11111100"
       "111000000" "111111111" "1110000111"]

      "012(34)*" 10 []

      "123(45*)*" 10
      ["123" "1234" "12344" "12345" "123444" "123445" "123454" "123455"]

      "10{5,}201705190{5,}1" 10
      ["10000020170519000001"
       "100000020170519000001"
       "100000201705190000001"
       "1000000020170519000001"
       "1000000201705190000001"
       "1000002017051900000001"
       "10000000020170519000001"
       "10000000201705190000001"
       "10000002017051900000001"
       "10000020170519000000001"]

      "(1234567|783|1444)*" 10
      ["783"
       "1444"
       "783783"
       "1234567"
       "1444783"
       "7831444"
       "14441444"
       "783783783"
       "1234567783"
       "1444783783"
       "7831234567"
       "7831444783"
       "7837831444"]

      ".*" 4
      ["1" "2" "3" "10" "11" "12" "13" "20"]

      "([31]|11){1,}." 7
      []

      ".*gary.*" 36
      []

      "201705190{7,}.*" 10
      ["201705190000000"
       "2017051900000000"
       "2017051900000001"
       "2017051900000002"
       "2017051900000003"
       "2017051900000004"
       "2017051900000005"
       "2017051900000006"]

      ".{26}" 10
      ["10000000000000000000000000"
       "10000000000000000000000001"
       "10000000000000000000000002"
       "10000000000000000000000003"
       "10000000000000000000000004"]

      ;; need to speed up the minimization of regexes like these
      "(.{10}){10}" 10
      ["1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
       "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"
       "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002"
       "1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003"]))

(def gen-modifier
  (gen/one-of [(gen/return "*")
               (gen/return "?")
               (gen/return "+")
               (gen/let [n gen/s-pos-int] (format "{%d}" n))
               (gen/let [n gen/s-pos-int] (format "{%d,}" n))
               (gen/let [pair (gen/vector gen/s-pos-int 2)]
                 (apply format "{%d,%d}" (sort pair)))]))

(defn modifiable?
  [s]
  (re-matches #".*[^\*\?\+\}\|]" s))

(defn gen-regex
  [alphabet]
  (gen/recursive-gen
   (fn [g]
     (gen/one-of
      [(gen/let [[s modifier] (gen/tuple g gen-modifier)]
         (if (modifiable? s)
           (str s modifier)
           s))
       (gen/let [s g] (str "(" s ")"))
       (gen/let [ss (gen/vector g)] (clojure.string/join "|" ss))]))
   (gen/one-of [(gen/elements alphabet)
                (gen/elements ["." "\\d" "\\D"])
                (gen/let [char-class-elems
                          (gen/not-empty
                           (gen/vector
                            (gen/one-of
                             [(gen/vector (gen/elements alphabet) 2)
                              (gen/elements alphabet)
                              (gen/elements ["\\d" "\\D"])])))
                          negation (gen/elements ["" "^"])]
                  (str "["
                       negation
                       (apply str
                              (for [elem char-class-elems]
                                (if (string? elem)
                                  elem
                                  (let [[a b] (sort elem)]
                                    (str a "-" b)))))
                       "]"))])))

(defspec nfa-determinism
  (prop/for-all [regex (gen-regex #{"0" "1" "2"})]
    (try
      (= (take 100 (regex/all-nonempty-matches (regex/compile regex 3)))
         (take 100 (regex/all-nonempty-matches (regex/compile regex 3))))
      (catch Exception e
        (or (re-find #"too much repetition" (.getMessage e))
            (throw e))))))

;; there's a rare bug that this spec occasionally catches, that to fix
;; will I think require a stuff/add-dead-state function to be called
;; prior to stuff/minimize-dfa, if we choose to continue using that.
;;
;; I think it's "((1{2})?)" in particular.
(defspec regex-parsing-etc-spec 1000
  (prop/for-all [regex (gen-regex #{"0" "1" "2"})]
    (try
      (let [nfa (regex/compile regex 3)
            jvm-regex (re-pattern regex)]
        (every? (fn [s] (and (nfa-matches? nfa s)
                             (re-matches jvm-regex s)))
                (take 100 (regex/all-nonempty-matches nfa))))
      (catch Exception e
        (or (re-find #"TOO MANY states" (.getMessage e))
            (throw e))))))

(defspec enumeration-completeness-spec 1000
  (prop/for-all [regex (gen-regex (set (map str "0123456789")))
                 seed gen/large-integer]
    (try
      (let [nfa (regex/compile regex 10)
            jvm-regex (re-pattern regex)
            matches (take 100 (regex/all-nonempty-matches nfa))]
        (or (empty? matches)
            (let [biggest-num (BigInteger. (last matches))

                  random-smaller-num
                  (str
                   (mod
                    (BigInteger. (* 2 (.bitLength biggest-num))
                                 (java.util.Random. seed))
                    biggest-num))]
              (= (boolean
                  (and (not= "0" random-smaller-num)
                       (re-matches jvm-regex random-smaller-num)))
                 (boolean (some #{random-smaller-num} matches))))))
      (catch Exception e
        (or (re-find #"TOO MANY states" (.getMessage e))
            (throw e))))))
