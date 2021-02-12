(ns intcode.ops
  (:require  [clojure.core.async :as a :refer [>!! <!!]]))

(defn- update-state [state & params]
  (apply assoc (cons state params)))

(defn- get-arg [param mode state]
  (case mode
    0 (get (state :insts) param)
    1 param))

(defn- get-args [n modes state]
  (let [insts-sub (->> (state :insts)
                       (drop (inc (state :ip)))
                       (take n))
        modes-sub (take n modes)]
    (map #(get-arg %1 %2 state) insts-sub modes-sub)))


;; ====== OPERATIONS ==========================================================


(defn- op-add
  [modes state]
  (let [args   (get-args 2 modes state)
        adr    ((state :insts) (+ (state :ip) 3))
        result (reduce + args)]

    (update-state state
                  :insts (assoc (state :insts) adr result)
                  :ip (+ (state :ip) 4))))

(defn- op-mul
  [modes state]
  (let [args   (get-args 2 modes state)
        adr    ((state :insts) (+ (state :ip) 3))
        result (reduce * args)]

    (update-state state
                  :insts (assoc (state :insts) adr result)
                  :ip (+ (state :ip) 4))))

(defn- op-inp
  [modes state]
  (let [input  (state :inp)
        adr    ((state :insts) (inc (state :ip)))
        result (<!! input)]

    (update-state state
                  :insts (assoc (state :insts) adr result)
                  :ip (+ (state :ip) 2))))

(defn- op-out
  [modes state]
  (let [output (state :out)
        [arg]  (get-args 1 modes state)]

    (>!! output arg)
    (update-state state
                  :ip (+ (state :ip) 2))))

(defn- op-jnz
  [modes state]
  (let [[arg trg] (get-args 2 modes state)
        pred      (not (zero? arg))]

    (update-state state
                  :ip (if pred trg (+ (state :ip) 3)))))

(defn- op-jez
  [modes state]
  (let [[arg trg] (get-args 2 modes state)
        pred      (zero? arg)]

    (update-state state
                  :ip (if pred trg (+ (state :ip) 3)))))

(defn- op-lst
  [modes state]
  (let [args (get-args 2 modes state)
        adr  ((state :insts) (+ (state :ip) 3))
        pred (apply < args)]

    (update-state state
                  :insts (assoc (state :insts) adr (if pred 1 0))
                  :ip (+ (state :ip) 4))))

(defn- op-equ
  [modes state]
  (let [args (get-args 2 modes state)
        adr  ((state :insts) (+ (state :ip) 3))
        pred (apply = args)]

    (update-state state
                  :insts (assoc (state :insts) adr (if pred 1 0))
                  :ip (+ (state :ip) 4))))

(defn get-op [opcode]
  (case opcode
    1 op-add
    2 op-mul
    3 op-inp
    4 op-out
    5 op-jnz
    6 op-jez
    7 op-lst
    8 op-equ
    nil))