(ns bespoke-primes.regex.stuff
  (:refer-clojure :exclude [contains? reverse])
  (:require [clojure.core :as core]
            [clojure.walk :as wk]
            [clojure.set :as sets]
            [clojure.string :as string]))

(def epsilon :ε)

(defprotocol IRanguage
  (contains? [r s] "returns true if the language contains the string/seq"))

(let [gs (comp keyword (partial gensym "s"))]
  (defn- new-state
    [{states :states}]
    (loop [s (gs)]
      (if (states s)
        (recur (gs))
        s))))

(defn- simplify-trans-map
  "Given a map with charset keys, unifies all keys that map to the
  same value (whether states or state-sets)."
  [m]
  ;; actually not sure that this version is faster O_O
  (loop [seen (transient {}) ;; map from vals to
         entries m
         m (transient m)]
    (if-let [[k v] (first entries)]
      (if-let [old-key (get seen v)]
        (let [new-key (into old-key k)]
          (recur (assoc! seen v new-key)
                 (rest entries)
                 (-> m
                     (dissoc! old-key k)
                     (assoc! new-key v))))
        (recur (assoc! seen v k) (rest entries) m))
      (persistent! m))))

(defn- unify-transition-functions
  "Given two maps from character-sets to state-sets, returns a single
  map that unifies them (i.e., combines the transitions. Whatever.)"
  [& tfs]
  (let [epsilon-states (->> tfs (map epsilon) (reduce sets/union))
        res
        (->> (for [tf tfs
                   [charset state-set] tf
                   :when (not= charset epsilon)
                   char charset]
               {#{char} state-set})
             (apply merge-with sets/union {})
             (simplify-trans-map))]
    (if (seq epsilon-states)
      (assoc res epsilon epsilon-states)
      res)))

(defn- nfa-transition
  "Returns the set of states that a particular state and character
  lead to."
  [nfa state c]
  (if (= epsilon c)
    (get-in nfa [:transitions state epsilon] #{})
    (let [inner ((:transitions nfa) state),
          charset (first (filter #(% c) (keys inner)))]
      (inner charset))))

(defn- nfa-epsilon-closure
  [nfa state]
  (loop [sts #{state}]
    (let [sts* (sets/union sts (set (mapcat #(nfa-transition nfa % epsilon) sts)))]
      (if (= sts sts*) sts (recur sts*)))))

(defn remove-epsilon-transitions
  "Returns an equvilant nfa with no epsilon transitions"
  [{:keys [transitions start accept] :as nfa}]
  (let [states (keys transitions)
        epsilon-closure (memoize (partial nfa-epsilon-closure nfa)),
        ;; add start state as accepting if necessary
        start-is-accept?
        (not (empty? (sets/intersection accept (set (epsilon-closure start))))),

        new-transitions
        (zipmap
         states
         (for [state states]
           (apply
            unify-transition-functions
            (for [state* (epsilon-closure state)]
              (let [t (dissoc (transitions state*) epsilon)]
                (zipmap
                 (keys t)
                 (for [v (vals t)]
                   (reduce sets/union (map epsilon-closure v)))))))))]
    (-> nfa
        (assoc :transitions new-transitions)
        (assoc :accept (if start-is-accept? (conj accept start) accept)))))

;; Represents an NFA with epsilon transitions. Because each
;; state/char maps to a subset of states, non-accepting paths
;; can be modeled as simply transition to the empty-set of states.
;;
;; Argument Types:
;;   states: set of keywords
;;   alphabet: set of anything? (or chars...)
;;   transitions:
;;     map from
;;       state-names
;;     to
;;       map from
;;         epsilon and subsets of the alphabet
;;       to
;;         sets of state names
;;   start: a state name
;;   accept: a set of state names
(defrecord NFA [states alphabet transitions start accept]
  IRanguage
  (contains? [n s]
    (not
     (empty?
      (sets/intersection
       accept
       (reduce
        (fn [sts c]
          (set
           (for [st sts,
                 new-st (nfa-transition n st c),
                 clsd-st (nfa-epsilon-closure n new-st)]
             clsd-st)))
        (nfa-epsilon-closure n start)
        s))))))
