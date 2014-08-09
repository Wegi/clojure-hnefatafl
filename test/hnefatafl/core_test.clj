(ns hnefatafl.core-test
  (:use midje.sweet)
  (:require [hnefatafl.core :refer :all]))

(fact "check board initialization"
  (((init-board) 5) 5) => :king
  (((init-board) 0) 5) => :black
  (((init-board) 5) 6) => :white
  (((init-board) 0) 0) => :castle
  (((init-board) 0) 10) => :castle)

(fact "Test the move mechanism"
  (get-in (move (init-board) :white-player [5 3] [9 3]) [9 3]) => :white
  (move (init-board) :white-player [5 1] [9 1]) => :wrong-piece
  (move (init-board) :black-player [5 1] [9 2]) => :move-forbidden
  (move (init-board) :shrek [5 1] [9 1]) => :not-a-player
  (get-in (move (init-board) :white-player [5 3] [9 3]) [5 3]) => :empty
  (move (init-board) :white-player [5 5] [8 5]) => :move-forbidden
  (move (init-board) :black-player [5 1] [11 1]) => :move-forbidden
  (move (init-board) :white-player [5 5] [5 2]) => :move-forbidden)

(fact "Test kill condition"
  (let [a (move (init-board) :black-player [5 1] [5 2])
        b (move a :black-player [10 3] [6 3])]
    (get-in b [5 3])) => :empty
  (get-in (move (init-board) :black-player [5 1] [6 1]) [6 0]) => :black
  (let [a (move (init-board) :black-player [3 0] [1 0])
        b (move a :black-player [0 3] [0 1])]
    (get-in b [0 0])) => :castle)

(fact "Test win-condition"
  (won? (assoc-in (init-board) [5 5] :throne)) => :black-player
  (won? (assoc-in (init-board) [0 0] :king)) => :white-player
  (won? (init-board)) => nil)
