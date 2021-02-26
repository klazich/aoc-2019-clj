(ns intcode.ops
  (:require  [clojure.core.async :as a :refer [>!! <!!]]))

(defn- update-state [state & params]
  (apply assoc (cons state params)))

(defn- get-arg [param mode state]
  (case mode
    0 (get (state :code) param)
    1 param
    2 (get (state :code) (+ (state :rb) param))))

(defn- get-adr [param mode state]
  (case mode
    0 param
    2 (+ (state :rb) param)))

(defn- get-params [n state]
  (->> (state :code)
       (drop (inc (state :ip)))
       (take n)))

;; ====== OPERATIONS ==========================================================

(defn- op-add
  [modes state log]
  (let [[m1 m2 m3] modes
        [p1 p2 p3] (get-params 3 state)
        x          (get-arg p1 m1 state)
        y          (get-arg p2 m2 state)
        adr        (get-adr p3 m3 state)
        result     (+ x y)]

    (when log (prn "ADD" [p1 p2 [p3]] [m1 m2 [m3]] [x y] result adr))

    (update-state state
                  :code (assoc (state :code) adr result)
                  :ip (+ (state :ip) 4))))

(defn- op-mul
  [modes state log]
  (let [[m1 m2 m3] modes
        [p1 p2 p3] (get-params 3 state)
        x          (get-arg p1 m1 state)
        y          (get-arg p2 m2 state)
        adr        (get-adr p3 m3 state)
        result     (* x y)]

    (when log (prn "MUL" [p1 p2 [p3]] [m1 m2 [m3]] [x y] result adr))

    (update-state state
                  :code (assoc (state :code) adr result)
                  :ip (+ (state :ip) 4))))

(defn- op-inp
  [modes state log]
  (let [m1     (first modes)
        p1     ((state :code) (inc (state :ip)))
        adr    (get-adr p1 m1 state)
        result (<!! (state :inp))]

    (when log (prn "INP" [[p1]] [[m1]] [] result adr))

    (update-state state
                  :code (assoc (state :code) adr result)
                  :ip (+ (state :ip) 2))))

(defn- op-out
  [modes state log]
  (let [m1 (first modes)
        p1 ((state :code) (inc (state :ip)))
        x  (get-arg p1 m1 state)]

    (when log (prn "OUT" [p1] [m1] [x] x :out))

    (>!! (state :out) x)
    (update-state state
                  :ip (+ (state :ip) 2))))

(defn- op-jnz
  [modes state log]
  (let [[m1 m2] modes
        [p1 p2] (get-params 2 state)
        x       (get-arg p1 m1 state)
        n       (get-arg p2 m2 state)
        pred    (not (zero? x))]

    (when log (prn "JNZ" [p1 p2] [m1 m2] [x n] pred :ip))

    (update-state state
                  :ip (if pred n (+ (state :ip) 3)))))

(defn- op-jez
  [modes state log]
  (let [[m1 m2] modes
        [p1 p2] (get-params 2 state)
        x       (get-arg p1 m1 state)
        n       (get-arg p2 m2 state)
        pred    (zero? x)]

    (when log (prn "JEZ" [p1 p2] [m1 m2] [x n] pred :ip))

    (update-state state
                  :ip (if pred n (+ (state :ip) 3)))))

(defn- op-lst
  [modes state log]
  (let [[m1 m2 m3] modes
        [p1 p2 p3] (get-params 3 state)
        x          (get-arg p1 m1 state)
        y          (get-arg p2 m2 state)
        adr        (get-adr p3 m3 state)
        pred       (< x y)]

    (when log (prn "LST" [p1 p2 [p3]] [m1 m2 [m3]] [x y] pred adr))

    (update-state state
                  :code (assoc (state :code) adr (if pred 1 0))
                  :ip (+ (state :ip) 4))))

(defn- op-equ
  [modes state log]
  (let [[m1 m2 m3] modes
        [p1 p2 p3] (get-params 3 state)
        x          (get-arg p1 m1 state)
        y          (get-arg p2 m2 state)
        adr        (get-adr p3 m3 state)
        pred       (= x y)]

    (when log (prn "EQU" [p1 p2 [p3]] [m1 m2 [m3]] [x y] pred adr))

    (update-state state
                  :code (assoc (state :code) adr (if pred 1 0))
                  :ip (+ (state :ip) 4))))

(defn- op-rbo
  [modes state log]
  (let [m1 (first modes)
        p1 ((state :code) (inc (state :ip)))
        n  (get-arg p1 m1 state)]

    (when log (prn "RBO" [p1] [m1] [n] (+ (state :rb) n) :rb))

    (update-state state
                  :ip (+ (state :ip) 2)
                  :rb (+ (state :rb) n))))

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
    9 op-rbo
    nil))