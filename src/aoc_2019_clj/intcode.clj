(ns intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! take! put! timeout go-loop]]

            [intcode.ops :refer [get-op]]))

(defn- build-state
  ([instructions pointer input output]
   {:insts instructions
    :ip    pointer
    :inp   input
    :out   output}))

(defn- parse-inst [state]
  (let [inst   ((state :insts) (state :ip))
        opcode (mod inst 100)
        modes  (quot inst 100)
        m1     (mod modes 10)
        m2     (mod (quot modes 10) 10)
        m3     (mod (quot modes 100) 10)]
    [opcode (get-op opcode) m1 m2 m3]))

(defn intcode [instructions]
  (fn [init]
    (let [input  (chan 1000)
          output (chan 1000)]
      (>!! input init)
      (loop [state (build-state instructions 0 input output)]
        (let [[opcode op-fn & modes] (parse-inst state)]
          (if (= opcode 99)
            (>!! output :done)
            (recur (op-fn modes state)))))
      [input output])))

(defn- chain [a b]
  (go-loop [x (<! a)]
    (when-not (= x :done)
      (>! b x)
      (recur (<! a)))))

(defn intcode-series [vm-image]
  (fn [xs init]
    (let [input  (chan 1000)
          output (reduce (fn [c x] (-> (vm-image x)
                                       (as-> [i o] (do (chain c i) o))))
                         input
                         xs)]
      (>!! input init)
      [input output])))

(defn vm-runner
  ([input output init] (vm-runner input output init prn))
  ([input output init out-fn]
   (>!! input init)
   (go-loop [x (<! output)]
     (out-fn x)
     (when-not (= x :done)
       (recur (<! output))))))

;; ====== RUNNING =============================================================


(defn- read-input [input]
  (with-open [rdr (io/reader input)]
    (->> rdr
         slurp
         (re-seq #"-?\d+")
         (map #(Integer/parseInt %))
         vec)))

(defn append-to-file
  [filename s]
  (spit filename s :append true))

;; ====== DAY 05 ==============================================================

(def input-5 (io/resource "day05/input.txt"))
(def instructions-5 (read-input input-5))
(def vm-image-5 (intcode instructions-5))

(defn runner-5 [init]
  (io/delete-file "./test.txt" 1)
  (let [[input output] (vm-image-5 init)]
    (go-loop [x (<! output)]
      (if (= x :done)
        (str "DONE " init)
        (do (append-to-file "./test.txt" (str x " "))
            (recur (<! output)))))))

(<!! (runner-5 1))

;; ====== DAY 07 ==============================================================

(def input-7 (io/resource "day07/input.txt"))
(def instructions-7 (read-input input-7))
(def vm-image-7 (intcode instructions-7))
(def vm-series-image-7 (intcode-series vm-image-7))

;; (let [[i o] (series-template [1 0 2 4 3])]
;;   (>!! i 0)
;;   (go (println (<! o)))
;;   nil)


(def amp-combos (combo/permutations (range 0 5)))

;; (defn start-take [i o]
;;   (put! i 0)
;;   (go (<! o)))

;; (let [log (chan 200)]
;;   ;; (io/delete-file "./log-7.txt" 1)

;;   ;; (go-loop [x (<! log)]
;;   ;;   (prn x)
;;   ;;   (when x
;;   ;;     (append-to-file "./log-7.txt" (apply str x "\n"))
;;   ;;     (recur (<! log))))

;;   (doseq [xs amp-combos]
;;     (let [[i o] (series-template xs)]
;;       (prn xs (start-take i o)))))

;; (doseq [xs amp-combos]
;;   (let [[i o] (series-template xs)]

;;     (prn xs (<!! (start-take i o)))))

