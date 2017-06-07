(ns bespoke-primes.regex
  (:refer-clojure :exclude [cat compile])
  (:require [bespoke-primes.regex.stuff :as stuff]
            [clojure.walk :as wk]
            [clojure.set :as sets]
            [clojure.string :as string]
            [com.gfredericks.compare :as compare]
            [com.gfredericks.exact :as e]
            [instaparse.core :as parse]
            [plumbing.core :as p]))

(def EMPTY_STRING_REGEX [:qmark #{}])

(def gen-state (let [a (atom -1)] #(swap! a inc)))

(def MAX_STATES 250)

(defn check-state-count
  [state-count]
  (when (< MAX_STATES state-count)
    (throw (ex-info "This state machine has TOO MANY states! Try a simpler regex." {:type :user-error}))))

(defn dispatch
  ([x]
   (if (set? x)
     (if (empty? x)
       :empty-set
       :set)
     (if (= EMPTY_STRING_REGEX x)
       :empty-string
       (first x))))
  ([x y] [(dispatch x) (dispatch y)]))

(def hierarchy
  (-> (make-hierarchy)
      (derive :set :any)
      (derive :qmark :any)
      (derive :star :any)
      (derive :cat :any)
      (derive :alt :any)
      (derive :empty-set :set)
      (derive :empty-string :qmark)))

(defn binary-prefer-method
  [multi [a b :as pair1] [c d :as pair2]]
  (prefer-method multi pair1 pair2)
  (prefer-method multi [b a] [d c]))

(defmulti matches-empty?
  "The length of the shortest matching string, or nil if
  there are no matching strings."
  dispatch
  :hierarchy #'hierarchy)
(defmulti to-pre-nfa dispatch :hierarchy #'hierarchy)
(defmulti qmark dispatch :hierarchy #'hierarchy)
(defmethod qmark :default [x] [:qmark x])
(defmulti star dispatch :hierarchy #'hierarchy)
(defmethod star :default [x] [:star x])
(defmulti cat dispatch :hierarchy #'hierarchy)
(defmethod cat :default [x y] [:cat [x y]])
(defmulti alt dispatch :hierarchy #'hierarchy)
(defmethod alt :default [x y] [:alt [x y]])

;; charsets
(defmethod matches-empty? :set [_] false)
(defmethod alt [:set :set] [x1 x2] (into x1 x2))
(defmethod to-pre-nfa :set
  [chars]
  {}
  (let [start (gen-state)]
    {:start start
     :accept-paths {start #{chars}}
     :transitions {start {chars #{:accept}}}}))

(defmethod to-pre-nfa :empty-set
  [_]
  (let [start (gen-state)]
    {:start start
     :accept-paths {}
     :transitions {start {}}}))
(defmethod to-pre-nfa :empty-string
  [_]
  (let [start (gen-state)]
    {:start start
     :accept-paths {start #{:ε}}
     :transitions {start {:ε #{:accept}}}}))

;; qmarks
(defmethod matches-empty? :qmark [_] true)
(defmethod qmark :qmark [this] this)
(defmethod star :qmark [[_ regex]] (star regex))
(defmethod to-pre-nfa :qmark
  [[_ regex]]
  (let [{:keys [start] :as nfa} (to-pre-nfa regex)]
    (-> nfa
        (update-in [:transitions (:start nfa) :ε]
                   (fnil conj #{})
                   :accept)
        (update-in [:accept-paths start] (fnil conj #{}) :ε))))

;; stars
(defmethod matches-empty? :star [_] true)
(defmethod qmark :star [this] this)
(defmethod star :star [this] this)
(defmethod to-pre-nfa :star
  [[_ regex]]
  (let [{:keys [start accept-paths] :as nfa} (to-pre-nfa regex)]
    (-> nfa
        (update-in [:transitions (:start nfa) :ε]
                   (fnil conj #{})
                   :accept)
        (update-in [:accept-paths start] (fnil conj #{}) :ε)
        (update :transitions
                (fn [transitions]
                  (reduce-kv (fn [transitions from-state charsets]
                               (update transitions from-state
                                       (fn [charsets->to-states]
                                         (reduce #(update %1 %2 (fnil conj #{}) start)
                                                 charsets->to-states
                                                 charsets))))
                             transitions
                             (cond-> accept-paths
                               (contains? accept-paths start)
                               (update start disj :ε))))))))

;; cats; assumes regexes is a vector of at least 2 regexes
(defmethod matches-empty? :cat
  [[_ regexes]]
  (every? matches-empty? regexes))
(defmethod cat [:cat :cat]
  [[_ regexes-1] [_ regexes-2]]
  [:cat (into regexes-1 regexes-2)])
(defmethod cat [:cat :any]
  [[_ regexes] regex]
  [:cat (conj regexes regex)])
(defmethod cat [:any :cat]
  [regex [_ regexes]]
  [:cat (into [regex] regexes)])
(defmethod cat [:any :empty-string] [regex _] regex)
(defmethod cat [:empty-string :any] [_ regex] regex)
(defmethod cat [:empty-string :empty-string] [regex _] regex)
(prefer-method cat [:any :empty-string] [:empty-string :any])
(binary-prefer-method cat [:any :empty-string] [:qmark :qmark])
(prefer-method cat [:empty-string :empty-string] [:qmark :qmark])
(binary-prefer-method cat [:empty-set :any] [:any :empty-string])
(binary-prefer-method cat [:empty-set :any] [:any :cat])
(binary-prefer-method cat [:empty-string :any] [:any :cat])
(prefer-method cat [:empty-set :any] [:any :empty-set])
(defmethod cat [:empty-set :any] [es _] es)
(defmethod cat [:any :empty-set] [_ es] es)

(defn map-to-pre-nfa-with-checking
  [regexes]
  (loop [total 0
         regexes regexes
         out (transient [])]
    (if (empty? regexes)
      (persistent! out)
      (let [nfa (to-pre-nfa (first regexes))
            total' (+ total (count (:transitions nfa)))]
        (check-state-count total')
        (recur total' (next regexes) (conj! out nfa))))))

(defmethod to-pre-nfa :cat
  [[_ regexes]]
  (let [nfas (map-to-pre-nfa-with-checking regexes)
        merged-transitions (->> nfas
                                (map :transitions)
                                (apply merge))]
    {:start (:start (first nfas))
     :accept-paths (:accept-paths (last nfas))
     :transitions
     (reduce (fn [transitions [nfa1 {:keys [start]}]]
               (reduce-kv (fn [transitions from-state charsets]
                            (update transitions from-state
                                    (fn [charsets->to-states]
                                      (reduce #(update %1 %2
                                                       (fn [to-states]
                                                         (-> to-states
                                                             (disj :accept)
                                                             (conj start))))
                                              charsets->to-states
                                              charsets))))
                          transitions
                          (:accept-paths nfa1)))
             merged-transitions
             (partition 2 1 nfas))}))

;; alts
(defmethod matches-empty? :alt
  [[_ regexes]]
  (some matches-empty? regexes))
(defmethod alt [:alt :alt]
  [[_ regexes-1] [_ regexes-2]]
  [:alt (into regexes-1 regexes-2)])
(defmethod alt [:alt :any]
  [[_ regexes] regex]
  [:alt (conj regexes regex)])
(defmethod alt [:any :alt]
  [regex [_ regexes]]
  [:alt (conj regexes regex)])
(defmethod alt [:any :empty-set] [regex _] regex)
(defmethod alt [:empty-set :any] [_ regex] regex)
(defmethod alt [:empty-set :empty-set] [regex _] regex)
(binary-prefer-method alt [:any :empty-set] [:set :set])
(prefer-method alt [:any :empty-set] [:empty-set :any])
(binary-prefer-method alt [:any :empty-set] [:empty-string :any])
(binary-prefer-method alt [:any :empty-set] [:alt :any])
(binary-prefer-method alt [:any :empty-string] [:alt :any])
(defmethod alt [:empty-string :empty-string] [regex _] regex)
(defmethod alt [:empty-string :any] [_ regex] (qmark regex))
(defmethod alt [:any :empty-string] [regex _] (qmark regex))

(defmethod to-pre-nfa :alt
  [[_ regexes]]
  (let [nfas (map-to-pre-nfa-with-checking regexes)
        start (gen-state)
        merged-transitions (->> nfas
                                (map :transitions)
                                (apply merge))]
    (check-state-count (inc (count merged-transitions)))
    ;; could save the extra state here by tediously merging the start
    ;; states.
    {:start start
     :accept-paths (->> nfas (map :accept-paths) (reduce into))
     :transitions (assoc merged-transitions
                         start {:ε (->> nfas (map :start) (set))})}))

(defn bounded-repetition
  "Between 0 and n repetitions of regex."
  [regex n]
  (qmark
   (case n
     0 #{}
     1 regex
     (cat regex (bounded-repetition regex (dec n))))))

(def ^:private parse-regex
  (parse/parser
   "ALTERNATION = CONCATENATION (<'|'> CONCATENATION) *
    CONCATENATION = (MODIFIED_EXPR | EXPR) *
    MODIFIED_EXPR = EXPR ('*' | '+' | '?' | REPETITION_RANGE)
    <REPETITION_RANGE> = CONSTANT_REPETITION | BOUNDED_REPETITION | UNBOUNDED_REPETITION
    CONSTANT_REPETITION = <'{'> NONNEGINT <'}'>
    BOUNDED_REPETITION = <'{'> NONNEGINT <','> NONNEGINT <'}'>
    UNBOUNDED_REPETITION = <'{'> NONNEGINT <',}'>
    NONNEGINT = #'0|[1-9][0-9]*'
    <EXPR> = CHAR | <'('> ALTERNATION <')'>
    <CHAR> = LITERAL | CHARCLASS | NEGATEDCHARCLASS | BACKSLASHCLASS | DOT
    NEGATEDCHARCLASS = <'[^'> CHARCLASSELEM+ <']'>
    CHARCLASS = <'['> CHARCLASSELEM+ <']'>
    <CHARCLASSELEM> = LITERAL | RANGE | BACKSLASHCLASS
    BACKSLASHCLASS = <'\\\\'> ('d' | 'D')
    RANGE = LITERAL <'-'> LITERAL
    DOT = <'.'>
    LITERAL = #'[0-9a-z]'
    "))

(def DIGITS (set (map str "0123456789")))

(defn ^:private transform
  [parse-tree alphabet]
  (parse/transform
   {:ALTERNATION (fn [& concats]
                   (reduce alt concats))
    :CONCATENATION (fn [& exprs]
                     (if (seq exprs)
                       (reduce cat exprs)
                       EMPTY_STRING_REGEX))
    :MODIFIED_EXPR (fn [expr modifier]
                     (case modifier
                       "*"
                       (star expr)

                       "?"
                       (qmark expr)

                       "+"
                       (cat expr (star expr))

                       (let [[type a b] modifier]
                         (check-state-count a)

                         (case type
                           :CONSTANT_REPETITION
                           (reduce cat (repeat a expr))

                           :BOUNDED_REPETITION
                           (if (< b a)
                             (throw (ex-info "Bad repetition range!" {:lb a :ub b}))
                             (reduce cat (concat (repeat a expr)
                                                 [(bounded-repetition expr (- b a))])))

                           :UNBOUNDED_REPETITION
                           (reduce cat (concat (repeat a expr)
                                               [(star expr)]))))))
    :NONNEGINT (fn [s]
                 (if (< 10 (count s))
                   (check-state-count 10000000)
                   #?(:cljs (js/parseInt s)
                      :clj (Long/parseLong s))))
    :CHARCLASS (fn [& lists+ranges]
                 (reduce into (sorted-set) lists+ranges))
    :NEGATEDCHARCLASS (fn [& lists+ranges]
                        (apply disj alphabet
                               (apply concat lists+ranges)))
    :RANGE (fn [begin end]
             (let [begin #?(:cljs (.charCodeAt (first begin) 0)
                            :clj (int (ffirst begin)))
                   end #?(:cljs (.charCodeAt (first end) 0)
                          :clj (int (ffirst end)))]
               (assert (<= begin end))
               (->> (range begin (inc end))
                    (map #?(:cljs #(String/fromCharCode %)
                            :clj (comp str char)))
                    (apply sorted-set))))
    :BACKSLASHCLASS (fn [d]
                      ((case d
                         "d" sets/intersection
                         "D" sets/difference)
                       alphabet
                       DIGITS))
    :DOT (constantly alphabet)
    :LITERAL (fn [char]
               (when-not (contains? alphabet char)
                 (throw (ex-info (str "Character not in base: " char) {})))
               (sorted-set char))}

   parse-tree))

(def alphabet
  (into {}
        (let [chars (map str "0123456789abcdefghijklmnopqrstuvwxyz")]
          (for [base (range 2 37)]
            [base (apply sorted-set (take base chars))]))))

(def disallowed-char-pattern
  #"[^0-9a-z\.{}\[\]\*\+\?\(\),\-\^\|\\D]")

(defn parse
  [regex-string base]
  (if-let [[char] (re-find disallowed-char-pattern regex-string)]
    (if (= " " char)
      (throw (ex-info "Whitespace is not allowed." {:type :parse-error
                                                    :input regex-string}))
      (throw (ex-info (str "Disallowed character: " char) {:type :parse-error
                                                           :input regex-string})))
    (let [parsed (parse-regex regex-string)]
      (if (parse/failure? parsed)
        (throw (ex-info (print-str parsed) {:type :parse-error}))
        (transform parsed (or (alphabet base)
                              (throw (ex-info "SHIT" {:base base}))))))))

(defn rang-dfa->DFA
  [{:keys [states alphabet transitions start accept] :as rang-dfa}]
  (let [state-> (zipmap (cons start (sort (remove #{start} states)))
                        (range))]
    {:accepting
     (set (map state-> accept))

     :transitions
     (into {}
           (for [[from-state m] transitions]
             [(state-> from-state)
              (into {}
                    (for [[charset to-state] m]
                      [(apply sorted-set charset)
                       (state-> to-state)]))]))}))

(defn nfa->rang-nfa
  [{:keys [start transitions accept]} alphabet]
  (stuff/map->NFA
   {:states (set (keys transitions))
    :alphabet alphabet
    :transitions transitions
    :start start
    :accept accept}))

(defn pre-nfa->nfa
  [{:keys [start transitions]} alphabet]
  (let [accept (gen-state)
        nonepsilon-accepts (->> transitions
                                (map (fn [[from-state m]]
                                       (filter #(contains? (val %) :accept)))))
        accepting-states (->> transitions
                              (keep (fn [[from-state m]]
                                      (if (contains? (m :ε) :accept)
                                        from-state)))
                              (set))
        transitions
        (into {}
              (for [[from-state m] transitions
                    :let [m' (into {}
                                   (for [[charset-or-ε to-states :as pair] m
                                         :when (not= [:ε #{:accept}] pair)]
                                     [charset-or-ε
                                      (if (contains? to-states :accept)
                                        (if (= :ε charset-or-ε)
                                          (disj to-states :accept)
                                          (-> to-states (disj :accept) (conj accept)))
                                        to-states)]))]]
                [from-state
                 (if (empty? m') {alphabet #{}} m')]))]

    (if (empty? nonepsilon-accepts)
      {:start start
       :accept accepting-states
       :transitions transitions}
      {:start start
       :accept (conj accepting-states accept)
       :transitions (assoc transitions
                           accept {alphabet #{}})})))

(defn nfa-without-initial-zeros
  [{:keys [start transitions accept] :as nfa}]
  (let [new-start (gen-state)]
    (assoc nfa
           :start new-start
           :accept (cond-> accept (contains? accept start) (conj new-start))
           :transitions (assoc transitions
                               new-start
                               (p/for-map [[charset to-states] (get transitions start)
                                           :let [charset' (disj charset "0")]
                                           :when (seq charset')]
                                 charset' to-states)))))

(defn compile
  "Compiles the regex, excluding initial zeros."
  [regex-string base]
  (-> regex-string
      (parse base)
      (to-pre-nfa)
      (pre-nfa->nfa (alphabet base))
      (nfa->rang-nfa (alphabet base))
      (stuff/remove-epsilon-transitions)
      (nfa-without-initial-zeros)))

(defn explode-transitions
  "Arg is an epsilon-free nfa transition function."
  [transitions]
  (p/map-vals
   (fn [charsets->-to-states]
     (->> charsets->-to-states
          (mapcat (fn [[charset to-states]] (map #(vector % to-states) charset)))
          (into (sorted-map))))
   transitions))

(defn ^:private nfa-all-matches-of-length
  [transitions start min-matches]
  (let [length (dec (count min-matches))
        func (fn [S c]
               (let [i (count S)]
                 (conj S
                       (->> (peek S)
                            (map #(get-in transitions [% c]))
                            (apply sets/union)
                            (filter #(get-in min-matches [(- length i) %]))
                            (set)))))]
    (when-let [triple (get (peek min-matches) start)]
      (->> (iterate (fn next-word [[S word]]
                      (reduce (fn [S char]
                                (if (empty? S)
                                  (reduced nil)
                                  (let [candidates
                                        (for [state (peek S)
                                              [char to-states] (subseq (get transitions state)
                                                                       > char)
                                              to-state to-states
                                              :let [triple
                                                    (get-in min-matches [(- length (count S)) to-state])]
                                              :when triple]
                                          [char triple])]
                                    (if (empty? candidates)
                                      (pop S)
                                      (reduced
                                       (let [next-char (->> candidates (map first) (apply compare/min))
                                             triple (->> candidates
                                                         (filter #(= next-char (first %)))
                                                         (apply min-key (comp last second)))
                                             [_next-char [min-match via-state sort-key]] triple]
                                         (assert (= next-char _next-char))
                                         [(reduce func S (cons next-char min-match))
                                          (into (subvec word 0 (dec (count S)))
                                                (cons next-char min-match))]))))))
                              (pop S)
                              (rseq word)))
                    (let [word (first triple)]
                      (assert (every? string? word))
                      [(reduce
                        func
                        [#{start}]
                        word)
                       (vec word)]))
           (map second)
           (take-while identity)))))

(defn stop-after-consecutive-nils
  ([nils coll] (stop-after-consecutive-nils nils coll nils))
  ([nils coll reset-to]
   (when (pos? nils)
     (lazy-seq
      (when-let [[x & more] (seq coll)]
        (cons x
              (stop-after-consecutive-nils
               (if (nil? x)
                 (dec nils)
                 reset-to)
               more
               reset-to)))))))

;; http://www.mayaackerman.info/pub/ThreeNewAlgorithmsForRegularLanEnum.pdf
(defn all-nonempty-matches
  "Arg must have had epsilon transitions removed."
  [{:keys [start transitions accept] :as nfa}]
  ;; The iteration is over a vector of path-hints, where
  ;; the index in the vector corresponds to the length of
  ;; the path.
  ;;
  ;; Each path-hint is a map from states to maybe a triple
  ;; [min-match via-state sort-key]
  ;; Where
  ;; - min-match is a sequence of characters
  ;; - via-state is the next state that the minimum
  ;;             match is accepted via
  ;; - sort-key  is a number that can be compared to
  ;;             other numbers in the same map and
  ;;             is a shortcut for comparing the
  ;;             min-match
  ;;
  ;; The triple is nil exactly when there is no accepting path of the
  ;; appropriate length from the state in question.
  (let [transitions (explode-transitions transitions)]
    (->> (iterate (fn [min-matches]
                    (let [path-length (count min-matches)]
                      (conj min-matches
                            (let [pairs
                                  (map (fn [[state chars-to-states]]
                                         ;; I think this could be assymptotically optimized
                                         (let [candidates
                                               (for [[char to-states] chars-to-states
                                                     to-state to-states
                                                     :let [[min-match via-state sort-key :as triple]
                                                           (get (peek min-matches) to-state)]
                                                     :when triple]
                                                 [char sort-key to-state min-match])]
                                           [state (if (not-empty candidates)
                                                    (apply compare/min candidates))]))
                                       transitions)
                                  char+sort-key->sort-key (zipmap (->> pairs
                                                                       (keep (fn [[state [char sort-key]]]
                                                                               (when char [char sort-key])))
                                                                       (into (sorted-set)))
                                                                  (range))]
                              (p/for-map [[state [char sort-key to-state min-match]] pairs]
                                state
                                (if char
                                  [(cons char min-match)
                                   to-state
                                   (char+sort-key->sort-key [char sort-key])]))))))
                  [(p/map-from-keys
                    (fn [state]
                      (if (accept state)
                        [() nil 0]))
                    (keys transitions))])
         (map #(nfa-all-matches-of-length transitions start %))
         (rest) ;; no empty matches
         (stop-after-consecutive-nils (count transitions))
         (apply concat)
         (map #(apply str %)))))
