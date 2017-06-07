(ns bespoke-primes.prod
  (:require [bespoke-primes.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
