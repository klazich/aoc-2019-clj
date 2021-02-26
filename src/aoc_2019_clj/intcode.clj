(ns intcode
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a :refer [>! <! >!! <!! go go-loop]]

            [intcode.vm :as vm]))

;; ============================================================================
;; ====== RUNNING =============================================================
;; ============================================================================

(defn read-input [input]
  (with-open [rdr (io/reader input)]
    (->> rdr
         slurp
         (re-seq #"-?\d+")
         (map #(Long/parseLong %))
         ((fn [v] (concat v (vec (repeat 1000000 0)))))
         vec)))

(defn append-to-file
  [filename s]
  (spit filename s :append true))

;; ============================================================================
;; ====== DAY 05 ==============================================================
;; ============================================================================

(def input-5 (io/resource "day05/input.txt"))
(def code-5 (read-input input-5))
(def image-5 (vm/build-image code-5))

(defn handle-io-5 [[_ output]]
  (go-loop [x    (<! output)
            last nil]
    (if (= x :done)
      last
      (do
        ;; (prn x)
        (recur (<! output) x)))))

(defn run-5 [init]
  (->> init
       image-5
       handle-io-5
       <!!))

(run-5 1)

;; ============================================================================
;; ====== DAY 07 ==============================================================
;; ============================================================================

(def input-7 (io/resource "day07/input.txt"))
(def code-7 (read-input input-7))
(def image-7 (vm/build-image code-7))

(defn handle-io-7-1 [[input output]]
  (go (<! output)))

(defn run-7-1 [xs init]
  (->> init
       ((vm/build-series-image image-7 xs))
       handle-io-7-1
       <!!))

;; (def amp-combos-1 (combo/permutations (range 0 5)))
;; (def max-7-1 (apply max (map #(run-7-1 % 0) amp-combos-1)))

(defn solve-7-1 []
  (let [amps (combo/permutations (range 0 5))]
    (apply max (map #(run-7-1 % 0) amps))))

;; PART TWO

(defn handle-io-7-2 [[input output]]
  (go-loop [x    (<! output)
            last nil]
    (if (= x :done)
      last
      (do (>! input x)
          (recur (<! output) x)))))

(defn run-7-2 [xs init]
  (->> init
       ((vm/build-series-image image-7 xs))
       handle-io-7-2
       <!!))

;; (def amp-combos-2 (combo/permutations (range 5 10)))
;; (def max-7-2 (apply max (map #(run-7-2 % 0) amp-combos-2)))

(defn solve-7-2 []
  (let [amps (combo/permutations (range 5 10))]
    (apply max (map #(run-7-2 % 0) amps))))

;; ============================================================================
;; ====== DAY 09 ==============================================================
;; ============================================================================

(def input-9 (io/resource "day09/input.txt"))
(def code-9 (read-input input-9))
(def image-9 (vm/build-image code-9))

(defn handle-io-9 [[_ output]]
  (io/delete-file "./log.txt" 1)
  (go-loop [x    (<! output)
            last nil]
    (if (= x :done)
      last
      (do
        ;; (prn x)
        (recur (<! output) x)))))

(defn run-9 [init]
  (->> init
       image-9
       handle-io-9
       <!!))

;; ============================================================================
;; ====== ASSERT ==============================================================
;; ============================================================================

(def test-intcode (every-pred (assert (= (run-5 1) 6745903) "DAY 5")
                              (assert (= (solve-7-1) 92663) "DAY 7-1")
                              (assert (= (solve-7-2) 14365052) "DAY 7-2")
                              (assert (= (run-9 1) 2789104029) "DAY 9-1")
                              ;; (assert (= (run-9 2) 32869) "DAY 9-2")
                              ))

(test-intcode)

;; ============================================================================
;; ====== DAY 11 ==============================================================
;; ============================================================================

