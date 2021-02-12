(ns intcode.vm
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
  ([code pointer input output]
   {:code code
    :ip   pointer
    :inp  input
    :out  output}))

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

(defn build-base-image [code]
  (fn [init]
    (let [input  (chan 1000)
          output (chan 1000)]

      (>!! input init)

      (go-loop [state (build-state code 0 input output)]
        (let [[opcode op-fn & modes] (parse-inst state)]
          (if (= opcode 99)
            (>! output :done)
            (recur (op-fn modes state))))))))