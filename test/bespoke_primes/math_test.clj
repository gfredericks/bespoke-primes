(ns bespoke-primes.math-test
  (:require [bespoke-primes.math :as math]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def gen-bigint
  (gen/let [xs (gen/not-empty (gen/vector (gen/large-integer* {:min 1})))]
    (+' (first xs) (apply *' (rest xs)))))

(defspec primality-spec 100
  (prop/for-all [n gen-bigint]
    (= (.isProbablePrime (biginteger n) 100)
       (trampoline math/prime? (str n) 10))))
