(ns bespoke-primes.regex.stuff-test
  (:require [bespoke-primes.regex.stuff :as stuff]
            [clojure.test :refer [deftest is]]))

(def bad-nfa
  #bespoke_primes.regex.stuff.NFA
  {:accept #{24130 24134 24135},
   :alphabet #{"1" "2"},
   :start 24128,
   :states #{24128 24129 24130 24131 24132 24133 24134 24135},
   :transitions
   {24128 {:ε #{24129 24131}, #{"1" "2"} #{}},
    24129 {#{"2"} #{24130}, #{"1"} #{}},
    24130 {#{"1" "2"} #{}},
    24131 {#{"1"} #{24132}, #{"2"} #{}},
    24132 {:ε #{24135}, #{"1" "2"} #{}},
    24133 {#{"1"} #{24134}, #{"2"} #{}},
    24134 {:ε #{24135}, #{"1" "2"} #{}},
    24135 {:ε #{24133}, #{"1" "2"} #{}}}})

(deftest regression-test-1
  (is (-> bad-nfa (stuff/contains? ["2"])))
  (is (-> bad-nfa stuff/remove-epsilon-transitions (stuff/contains? ["2"]))))
