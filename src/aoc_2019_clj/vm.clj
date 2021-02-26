(ns intcode.vm
  (:require [clojure.core.async :as a :refer [>! <! >!! <!! go chan go-loop]]

            [intcode.ops :refer [get-op]]))

(defn- build-state
  ([code pointer input output]
   (build-state code pointer 0 input output))
  ([code pointer relative-base input output]
   {:code code
    :ip   pointer
    :rb   relative-base
    :inp  input
    :out  output}))

(defn- parse-inst [state]
  (let [inst   ((state :code) (state :ip))
        opcode (mod inst 100)
        modes  (quot inst 100)
        m1     (mod modes 10)
        m2     (mod (quot modes 10) 10)
        m3     (mod (quot modes 100) 10)]
    [opcode (get-op opcode) m1 m2 m3]))

(defn chain [a b]
  (go-loop [x (<! a)]
    (when-not (= x :done)
      (>! b x)
      (recur (<! a)))))

(defn build-image
  ([code] (build-image code false))
  ([code with-log]
   (fn [init & {:as opt}]
     (let [code   (if (:sub opt)
                    (apply assoc code (:sub opt))
                    code)
           input  (chan 1000)
           output (chan 1000)]

       (when-not (nil? init) (>!! input init))

       (go-loop [state (build-state code 0 0 input output)]
         (let [[opcode op-fn & modes] (parse-inst state)]
           (if (= opcode 99)
             (>! output :done)
             (recur (op-fn modes state with-log)))))

       [input output]))))

(defn build-series-image
  ([image xs]
   (fn [init]
     (let [input  (chan 1000)
           output (reduce (fn [c x] (-> (image x)
                                        (as-> [i o] (do (chain c i) o))))
                          input
                          xs)]

       (>!! input init)

       [input output]))))

