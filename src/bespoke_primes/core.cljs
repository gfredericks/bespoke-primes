(ns bespoke-primes.core
  (:require [bespoke-primes.views :as views]
            [reagent.core :as reagent]))


;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [views/main-panel] (.getElementById js/document "app")))

(defn init! []
  (mount-root)
  (views/init!))
