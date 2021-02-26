(ns intcode.droid
  (:require [clojure.java.io :as io]
            [clojure.core.async :as a :refer [>! <! >!! <!! go chan go-loop]]
            [clojure.pprint :as pp]
            [clojure.string :refer [join]]

            [intcode :refer [read-input]]
            [intcode.vm :as vm]))

(def input (io/resource "day15/input.txt"))
(def code (read-input input))
(def image (vm/build-image code))

(defn- init-state []
  {:x    0
   :y    0
   :grid {[0 0] {:v 9
                 :c 1}}})

(defn- update-coords [state]
  (let [offset-x (Math/abs (apply min (map #(first %) (keys (:grid state)))))
        offset-y (Math/abs (apply min (map #(second %) (keys (:grid state)))))]

    (assoc state
           :x (+ (:x state) offset-x)
           :y (+ (:y state) offset-y)
           :grid (reduce-kv
                  (fn [m [x y] v] (assoc m
                                         [(+ x  offset-x) (+ y  offset-y)]
                                         v))
                  {}
                  (:grid state)))))

(defn- reset-c [state]
  (assoc state
         :grid (map (fn [[k v]]
                      (if-not (= (:v v) 2)
                        [k (assoc v :c 0)]
                        [k v])) (:grid state))))

(defn- grid->str [{x    :x
                   y    :y
                   grid :grid}]

  (let [rows (inc (apply max (map #(second %) (keys grid))))
        cols (inc (apply max (map #(first %) (keys grid))))
        vc   (reduce-kv (fn [vv [x y] {v :v}] (assoc-in vv [y x] v))
                        (vec (repeatedly rows #(vec (repeat cols 0))))
                        grid)
        disp (map-indexed (fn [i row]
                            (map-indexed (fn [j v]
                                           (if (= [x y] [j i])
                                             "@"
                                             (case v 1 " " 2 "█" 3 "F" 9 "S" "░")))
                                         row))
                          vc)]

    (join "\n" (map #(join "" %) disp))))

(defn m-dist [ax ay bx by]
  (+ (Math/abs (- ax bx))
     (Math/abs (- ay by))))

(def tile-default {:v 0
                   :c 0})
(defn- move-comp [{grid :grid}]
  (fn [A B]
    (let [{Av :v
           Ac :c} (get grid A tile-default)
          {Bv :v
           Bc :c} (get grid B tile-default)]

      (if (zero? (compare Av Bv))
        (compare Ac Bc)
        (compare Av Bv)))))

(defn- move-order
  ([state]
   (move-order state nil))
  ([state target]
    (print "move-order")
   (let [;; state (update-coords state)
         x     (:x state)
         y     (:y state)
         adj   [[(inc x) y 4] [(dec x) y 3] [x (inc y) 1] [x (dec y) 2]]
         moves (->> adj
                    (filter #(not= (get-in state [:grid (take 2 %) :v] 0)
                                   2))
                    (sort (move-comp state)))]

     (if target (sort (fn [A B] (compare (apply m-dist target A)
                                         (apply m-dist target B)))
                      moves)
         moves))))

(defn- display [state]
  (io/delete-file "./droid.txt" true)
  (spit "./droid.txt" (->> state
                           ;; update-coords
                           grid->str)))

(defn- inc-c
  ([state]
   (inc-c state (:x state) (:y state)))
  ([state x y]
   (update-in state [:grid [x y] :c] (fn [v] (if (nil? v) 1 (inc v))))))

(defn init-search []
  (print 1)
  (let [[in out] (image nil)]
    
    (print 2)

    (loop [state (init-state)]

      (let [moves         (move-order state)
            [x y request] (first moves)
            result        (do (>!! in request) (<!! out))]

        (if (= result 2)
          (do (display state)
              [in out state])

          (recur (-> state
                     (assoc-in [:grid [x y] :v] (case result
                                                  0 2
                                                  1 1
                                                  2 3))
                     (assoc :x (case result 0 (:x state) x))
                     (assoc :y (case result 0 (:y state) y))
                     inc-c
                     update-coords)))))
    
    ))

;; (defn shortest-path-back [[in out state]]
;;   (let [target (some (fn [[k v]] (when (= (:v v) 9) k)) (:grid state))]))

(init-search)

;; (some (fn [[k v]] (when (= (:v v) 9) k)) (get-in (init-search) [2 :grid]))

