(ns intcode
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! take! put! timeout]]))

(defn- get-value [mem param mode]
  (case mode
    0 (get mem param)
    1 param))

(defn- build-state
  ([instructions input output pointer]
   {:insts instructions
    :inp   input
    :out   output
    :ip    pointer}))

(defn- destruc-state [state]
  [(state :insts) (state :ip) (state :inp) (state :out)])

(defn- update-state [state & params]
  (apply assoc (cons state params)))

(defn- op-ADD ([modes state] (op-ADD 3 modes state))
  ([n modes state]
   (let [[insts ip inp out] (destruc-state state)
         raw-params         (subvec insts (inc ip) (+ ip 1 n))
         [x y trg]          (map #(get-value insts %1 %2) raw-params modes)
         result             (+ x y)]
     (pprint trg)
     (update-state state
                   :insts (assoc insts trg result)
                   :ip    (+ ip n 1)))))

(defn- op-MUL ([modes state] (op-ADD 3 modes state))
  ([n modes state]
   (let [[insts ip inp out] (destruc-state state)
         raw-params         (subvec insts (inc ip) (+ ip 1 n))
         [x y trg]          (map #(get-value insts %1 %2) raw-params modes)
         result             (* x y)]

     (update-state state
                   :insts (assoc insts trg result)
                   :ip    (+ ip n 1)))))

(defn- op-INP ([modes state] (op-INP 1 modes state))
  ([n modes state]
   (let [[insts ip inp out] (destruc-state state)
         raw-params         (subvec insts (inc ip) (+ ip 1 n))
         [trg]              (map #(get-value insts %1 %2) raw-params modes)
         result             (<!! inp)]

     (update-state state
                   :insts (assoc insts trg result)
                   :ip    (+ ip n 1)))))

(defn- op-OUT ([modes state] (op-INP 1 modes state))
  ([n modes state]
   (let [[insts ip inp out] (destruc-state state)
         raw-params         (subvec insts (inc ip) (+ ip 1 n))
         [src]              (map #(get-value insts %1 %2) raw-params modes)]

     (>!! out src)
     (update-state state
                   :ip (+ ip n 1)))))

(defn- parse-inst [inst]
  (let [opcode (rem inst 100)
        modes  (quot inst 100)]
    (pprint [[inst] opcode
             (bit-and modes 2r001)
             (bit-and modes 2r010)
             (bit-and modes 2r100)])
    [opcode
     (bit-and modes 2r001)
     (bit-and modes 2r010)
     (bit-and modes 2r100)]))

(defn- get-op [opcode]
  (case opcode
    1 op-ADD
    2 op-MUL
    3 op-INP
    4 op-OUT
    ; 5 op-jnz
    ; 6 op-jez
    ; 7 op-lst
    ; 8 op-equ
    nil))

(defn- Intcode
  ([instructions]
   (let [input  (chan 1000)
         output (chan 1000)]
     [(fn []
        (loop [state (build-state instructions input output 0)]
          (let [inst             ((state :insts) (state :ip))
                [opcode & modes] (parse-inst inst)
                op-fn            (get-op opcode)]
            (when-not (nil? op-fn)
              (recur (op-fn modes state))))))
      input
      output])))

(def input (io/resource "day05/input.txt"))

(defn- read-input [input]
  (with-open [rdr (io/reader input)]
    (->> rdr
         slurp
         (re-seq #"\d+")
         (map #(Integer/parseInt %))
         vec)))

(def instructions (read-input input))

(defn execute [init vm]
  (let [[runner input output] vm]
    (go (>! input init))
    (go (while true (pprint (<! output))))
    (runner)))

(execute 1 (Intcode instructions))

(pprint (take 10 instructions))
(pprint (count instructions))