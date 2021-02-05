(ns aoc-2019-clj.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! take! put! timeout]])
  (:gen-class))

; (defn foo
;   "I don't do a whole lot."
;   [x]
;   (println x "Hello, World!"))
