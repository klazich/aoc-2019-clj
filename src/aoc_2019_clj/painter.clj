(ns aoc-2019-clj.painter
  (:require [clojure.java.io :as io]
            [clojure.core.async :as a :refer [>! <! >!! <!! go chan go-loop]]

            [intcode :refer [read-input]]
            [intcode.vm :as vm]))

(defn build-state
  ([vm] (build-state vm 0 0 0 {}))
  ([vm x y heading grid] {:vm      vm
                          :x       x
                          :y       y
                          :heading heading
                          :grid    grid}))

(defn update-state [state & params]
  (apply assoc (cons state params)))

(defn make-move [state]
  (let [{x       :x
         y       :y
         heading :heading} state]
    (update-state state
                  :x (case heading
                       1 (inc x)
                       3 (dec x)
                       x)
                  :y (case heading
                       0 (inc y)
                       2 (dec y)
                       y))))

(defn make-turn [state d]
  (update-state state
                :heading (rem (+ 4
                                 (state :heading)
                                 (if (zero? d) -1 1))
                              4)))

(defn read-tile [state]
  (let [x    (state :x)
        y    (state :y)
        grid (state :grid)]

    (get grid [x y] 0)))

(defn paint-tile [state color]
  (update-state state
                :grid (assoc (state :grid)
                             [(state :x) (state :y)]
                             color)))

(defn- read-output [{[_ o] :vm}]
  (let [x (<!! o)]
    (if (= x :done)
      [:done nil]
      (let [y (<!! o)]
        (if (= y :done)
          [:done nil]
          [x y])))))

(defn- input-color [{[i] :vm} v]
  (>!! i v))

(defn- next-state [state]
  (input-color state (read-tile state))
  (let [[color turn] (read-output state)]
    ;; (prn state)
    (-> state
        (paint-tile color)
        (make-turn turn)
        make-move)))

;; ============================================================================
;; ====== RUNNING =============================================================
;; ============================================================================


(def input (io/resource "day11/input.txt"))
(def code (read-input input))
(def image (vm/build-image code))

(defn run-painter [image]
  (let [[input output]  (image nil)]

    (go-loop [state (build-state [input output])
              c     0]

      (input-color state (read-tile state))

      (let [[color-to-paint to-turn] (read-output state)]

        (when (zero? (rem c 100)) (prn c))

        (if (or (= color-to-paint :done) (nil? color-to-paint))
          (count (state :grid))
          (recur (-> state
                     (paint-tile color-to-paint)
                     (make-turn to-turn)
                     make-move)
                 (inc c)))))))

(<!! (run-painter image))

