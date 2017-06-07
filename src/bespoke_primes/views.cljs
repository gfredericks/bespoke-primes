(ns bespoke-primes.views
  (:require [cljs.pprint :as pprint]
            [com.gfredericks.exact :as e]
            [bespoke-primes.math :as math]
            [bespoke-primes.regex :as regex]
            [reagent.core :as r]
            [instaparse.core :as parse]))

(defn get-hash
  []
  (-> js/window
      .-location
      .-hash
      (subs 1)
      (js/decodeURIComponent)))

(defn set-hash
  [s]
  (set! (.-hash (.-location js/window))
        (js/encodeURIComponent s)))

(defn read-input-from-hash
  []
  (if-let [[_ regex base] (re-matches #"([^:]*):(any|[12][0-9]|3[0-6]|[2-9])"
                                      (get-hash))]
    {:regex regex
     :base (if (= "any" base) :any (js/parseInt base))}))

(defn write-input-to-hash
  [{:keys [regex base]}]
  (set-hash (str regex ":" (if (= :any base) "any" base))))

(defn generate-init-regex
  []
  ((rand-nth
    [(fn []
       (let [d (js/Date.)
             ;; hoping these are in the local time zone
             y (+ 1900 (.getYear d))
             m (inc (.getMonth d))
             d (.getDate d)
             m (if (= 1 (count (str m))) (str "0" m) m)
             d (if (= 1 (count (str d))) (str "0" d) d)]
         (str "11?0{5,15}" y m d "0{5,15}11?")))
     #(rand-nth ["(7[53]){7,9}7"
                 "(100?){9}1"
                 "10{20}[48]{8}0{20}1"])])))

(defonce input-state
  (r/atom
   {:regex ""
    :base 10}))

(defonce search-state
  (r/atom
   {:ui-state :done}))

(def search-state-machine
  {:searching        {:finished :done
                      :playpause :paused
                      :edit :compiling}
   :done             {:edit :compiling}
   :paused           {:playpause :searching
                      :edit :compiling-paused}
   :compiling        {:success :searching
                      :error :error}
   :compiling-paused {:success :paused-new
                      :error :error-paused}
   :paused-new       {:playpause :searching
                      :edit :compiling-paused}
   :error            {:edit :compiling}
   :error-paused     {:edit :compiling-paused}})

(defn next-state
  [current-state event]
  (get-in search-state-machine [current-state event] current-state))

(defn current-time-millis
  []
  (.valueOf (js/Date.)))

(defn time-limited-trampoline
  ([f] (time-limited-trampoline f (current-time-millis)))
  ([f start-time]
   (loop [f f]
     (if (or (not (ifn? f))
             (< 150 (- (current-time-millis) start-time)))
       f
       (recur (f))))))

(defn thinking
  [search-id]
  (let [{:keys [ui-state matches so-far primes id continue]}
        @search-state]
    (when (= search-id id)
      (if (not= :searching ui-state)
        (swap! search-state assoc :resume #(thinking search-id))
        (if (empty? matches)
          (swap! search-state update :ui-state next-state :finished)
          (do
            (let [[s base] (first matches)
                  res
                  (time-limited-trampoline
                   (or continue #(math/prime? s base)))]
              (if (ifn? res)
                (swap! search-state
                       #(-> %
                            (assoc :continue res)
                            (update :continue-steps (fnil inc 0))))
                (do
                  (when res
                    (swap! search-state update :primes conj [s base]))
                  (swap! search-state
                         (fn [search]
                           (-> search
                               (update :matches rest)
                               (update :so-far inc)
                               (dissoc :continue)))))))

            (js/setTimeout #(thinking search-id) 10)))))))

(defn start-thinking
  [search-id]
  (js/setTimeout #(thinking search-id) 10))

(def largest-char->smallest-base
  (delay
   (->> (map vector "123456789abcdefghijklmnopqrstuvwxyz" (drop 2 (range)))
        (into {}))))

(defn all-possible-bases
  "Returns a collection of [s base] for all bases in which s is a
  valid number."
  [s]
  (let [largest-char (reduce #(if (neg? (compare %1 %2)) %2 %1) s)]
    (for [base (range (@largest-char->smallest-base largest-char) 37)]
      [s base])))

(defn new-regex
  [{:keys [regex base]}]
  (let [parse-base (if (= base :any) 36 base)
        parsed (try
                 (regex/compile regex parse-base)
                 (catch :default e [:thrown e]))]
    (if (and (vector? parsed)
             (= :thrown (first parsed)))
      (swap! search-state (fn [state]
                            {:error (second parsed)
                             :id ((fnil inc 0) (:id state))
                             :ui-state (next-state (:ui-state state) :error)}))
      (let [{search-id :id}
            (swap! search-state
                   (fn [state]
                     {:matches (->> parsed
                                    (regex/all-nonempty-matches)
                                    (mapcat (if (= :any base)
                                              all-possible-bases
                                              (fn [s]
                                                [[s base]])))
                                    (lazy-seq))
                      :ui-state (next-state (:ui-state state) :success)
                      :so-far 0
                      :primes []
                      :error nil
                      :id ((fnil inc 0) (:id state))}))]
        (start-thinking search-id)))))

(add-watch input-state :thinker
           (fn [_ _ old new]
             (when (not= old new)
               (swap! search-state update :ui-state next-state :edit)
               (write-input-to-hash new)
               (js/setTimeout #(new-regex new) 5))))

(defn input-component
  []
  (let [{:keys [regex base]} @input-state]
    [:div.regex-form
     [:p "Enter a (basic) regular expression."]
     [:input {:type "text" :value regex
              :on-change #(swap! input-state assoc :regex
                                 (-> % .-target .-value))}]
     [:select
      {:value (str base)
       :on-change (fn [ev]
                    (let [value (-> ev .-target .-value)
                          new-base (if (= ":any" value) :any (js/parseInt value))]
                      (swap! input-state assoc :base new-base)))}
      (for [[b txt] (cons [:any "Any Base"] (map (juxt identity #(str "Base " %))
                                                 (range 2 37)))]
        [:option {:key (str b) :value (str b)} txt])]]))

(defn number-component
  [[number-string base] config-base]
  [:span.number
   [:code.number number-string
    (if-not (= 10 config-base)
      [:sub base])]])

(defn search-component
  []
  (let [{:keys [matches resume ui-state so-far error primes] :as state}
        @search-state
        {:keys [base]} @input-state]
    (if error
      [:pre.error (.-message error)]
      [:div.search
       [:h3 "Searching"]
       (let [ui-state->button-img
             #(case %
                :searching :pause
                (:paused :paused-new) :play
                nil)]
         (if-let [button-img (ui-state->button-img ui-state)]
           [:button#onoffbutton
            {:onClick (fn [ev]
                        (let [new-state (swap! search-state update :ui-state
                                               next-state :playpause)]
                          (if (= :searching (:ui-state new-state))
                            (if-let [f (get new-state :resume)]
                              (js/setTimeout f 5)))))}
            [:img {:src (str "bespoke-primes/" (name button-img) ".transparent.png")
                   :width "20px" :height "20px"}]]))
       (case ui-state
         (:searching :paused)
         [:span (case ui-state :paused {:class "paused"} {})
          (str " [" so-far "]: ")
          [number-component (first matches) base]
          (if (pos? (:continue-steps state))
            [:span " "
             (["◐" "◓" "◑" "◒"]
              (if-let [ct (:continue-steps state)]
                (mod ct 4)
                0))])]

         :paused-new " (search is paused)"

         :done
         [:span "Found "
          [:code (count primes)]
          " primes out of "
          [:code so-far]
          (if (= 1 so-far)
            " match."
            " matches.")]

         nil)
       [:h3 "Matching primes found"]
       [:ol {:reversed "true"}
        (for [[idx num] (reverse (map vector (range) primes))]
          [:li {:key (pr-str num)}
           [number-component num base]])]])))

(defn debug-component
  []
  [:div
   [:button {:on-click
             #(println {:search (dissoc @search-state :matches :primes) :input @input-state})}
    "Debug"]])

(defn main-panel
  []
  [:div
   [input-component]
   [:hr {:style {:margin-top "1em"
                 :margin-bottom "1em"}}]
   [search-component]])

(defn init!
  []
  (reset! input-state
          (or (read-input-from-hash)
              (doto {:regex (generate-init-regex)
                     :base 10}
                (write-input-to-hash))))
  (.addEventListener js/window "hashchange"
                     (fn []
                       (if-let [x (read-input-from-hash)]
                         (reset! input-state x)))))
