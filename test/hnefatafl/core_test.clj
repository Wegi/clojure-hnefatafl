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
  (move (init-board) :black-player [5 1] [11 1]) => :move-forbidden)

