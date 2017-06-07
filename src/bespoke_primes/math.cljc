(ns bespoke-primes.math
  (:require [com.gfredericks.exact :as e]))

(defn trampoline-bind
  [a-trampoline func]
  (let [x (a-trampoline)]
    (if (ifn? x)
      #(trampoline-bind x func)
      (func x))))

(def TWO (e/inc e/ONE))
(def SIXTY_FIVE_THOUSAND (e/native->integer 0x100000000))

(defn uniform-random
  [upper-exclusive]
  (let [big-num (->> (iterate (fn [x]
                                (-> x
                                    (e/* SIXTY_FIVE_THOUSAND)
                                    (e/+ (-> 0x10000
                                             rand-int
                                             #?@(:cljs () :clj (long))
                                             e/native->integer))))
                              e/ZERO)
                     (drop-while #(e/< % upper-exclusive))
                     (drop 4)
                     (first))]
    (e/mod big-num upper-exclusive)))

(defn mod-pow
  "Returns a^b mod m, or a trampoline for it. All args are Integers."
  [a b m]
  ;; could use .getBits on b instead of shifting; probably dwarfed by the
  ;; multiplication and modding though
  ((fn f [squared b res]
     (if (e/zero? b)
       res
       (partial f
                (e/mod (e/* squared squared) m)
                #?(:cljs (.shiftRight b 1)
                   :clj (quot b 2))
                (cond-> res
                  (e/odd? b)
                  (-> (e/* squared) (e/mod m))))))
   a b e/ONE))

;; https://github.com/openjdk-mirror/jdk/blob/jdk8u/jdk8u/master/src/share/classes/java/math/BigInteger.java
(defn miller-rabin-checker
  "Returns a function you can call as many times as you please.
  Each call may return a trampoline, but eventually returns "
  [pp]
  (let [pp-1 (e/dec pp)
        [a pp-1-odd]
        (loop [a e/ZERO
               pp-1-odd pp-1]
          (if (e/even? pp-1-odd)
            (recur (e/inc a) #?(:cljs (.shiftRight pp-1-odd 1)
                                :clj (quot pp-1-odd 2)))
            [a pp-1-odd]))]
    (fn []
      (let [b (-> pp
                  (e/- TWO)
                  (uniform-random)
                  (e/+ TWO))]
        (trampoline-bind
         #(mod-pow b pp-1-odd pp)
         (fn [z]
           ((fn f [j z]
              (if (or
                   (and (e/zero? j) (= e/ONE z))
                   (= z pp-1))
                true
                (let [j+1 (e/inc j)]
                  (if (or (and (e/pos? j)
                               (= e/ONE z))
                          (= a j+1))
                    false
                    #(f j+1 (e/mod (e/* z z) pp))))))
            e/ZERO z)))))))

(def TWENTY (e/native->integer 20))
(def SMALL_PRIMES
  (set [2 3 5 7 11 13 17 19]))

(defn prime?
  "Returns a boolean or a trampolining function that eventually
  returns a boolean."
  [s base]
  (let [n (e/string->integer s base)]
    (cond
      (e/< n TWENTY) (contains? SMALL_PRIMES (e/integer->native n))
      (e/even? n) false
      :else
      (let [f (miller-rabin-checker n)]
        ((fn yo [checks-left]
           (if (zero? checks-left)
             (f)
             (trampoline-bind
              f
              (fn [x]
                (case x
                  true #(yo (dec checks-left))
                  false false)))))
         100)))))
