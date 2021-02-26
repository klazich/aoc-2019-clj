(ns intcode.arcade
  (:require [clojure.java.io :as io]
            [clojure.core.async :as a :refer [>! <! >!! <!! go chan go-loop]]
            [clojure.pprint :as pp]
            [clojure.string :refer [join]]

            [intcode :refer [read-input]]
            [intcode.vm :as vm]))

;; 0  is an empty tile. No game object appears in this tile.
;; 1 █ is a wall tile. Walls are indestructible barriers.
;; 2 X is a block tile. Blocks can be broken by the ball.
;; 3 ━ is a horizontal paddle tile. The paddle is indestructible.
;; 4 ∙ is a ball tile. The ball moves diagonally and bounces off objects.

(defn pixel-stream [c]
  (let [out (chan 1000)]
    (go-loop [v     (<! c)
              pixel []]

      (when (= (count pixel) 2)
        (>! out (conj pixel
                      (if (not= pixel [-1 0])
                        (case v 0 " " 1 "█" 2 "X" 3 "━" 4 "∙" v)
                        v))))

      (if (= v :done)
        (>! out :done)
        (recur (<! c)
               (if (= (count pixel) 2)
                 []
                 (conj pixel v)))))

    out))

(defn print-img [img n]
  (io/delete-file "./arcade.txt" true)
  (spit "./arcade.txt" (str n "\n"))
  (let [str (join "\n" (map #(join "" %) img))]
    (spit "./arcade.txt" str :append true)))

(defn matrix
  ([] (matrix 42 26))
  ([x y] (vec (repeatedly y #(vec (repeat x " "))))))

(defn handle-screen
  ([stream] (handle-screen (matrix) stream))
  ([img stream]

   (go-loop [pixel (<! stream)
             score 0
             img   img]

     (if (= pixel :done)

       :done

       (let [[x y v] pixel
             n-score (if (= [x y] [-1 0]) v score)
             n-img   (if (= [x y] [-1 0])
                       img
                       (assoc-in img [y x] v))]

         (print-img n-img n-score)

         (recur (<! stream)
                n-score
                n-img))))))

(def input (io/resource "day13/input.txt"))
(def code (read-input input))
(def image (vm/build-image code))

(let [[i o]  (image nil)
      stream (pixel-stream o)]
  (go-loop [] (>! i (read-line)) (recur))
  (handle-screen stream))

;;

(defn refresh-img [image stream]
  (go-loop [pixel (<! stream)
            img   image]

    (if (= pixel :done)
      img
      (recur (<! stream)
             (assoc img [(first pixel) (second pixel)] (last pixel))))))

(defn img->str [img]
  (let [cols (apply max (map (fn [[x _]] x) (keys img)))
        rows (apply max (map (fn [[_ y]] y) (keys img)))]

    (loop [x       0
           y       0
           img-str ""]

      (let [v (get img [x y])]

        (if (> y rows)
          (str (get img [-1 0]) "\n" img-str)
          (recur (if (>= x cols) 0 (inc x))
                 (if (>= x cols) (inc y) y)
                 (if (>= x cols) (str img-str v "\n") (str img-str v))))))))
