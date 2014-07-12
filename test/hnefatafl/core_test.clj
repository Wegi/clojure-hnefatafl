(ns hnefatafl.core-test
  (:use midje.sweet)
  (:require [hnefatafl.core :refer :all]))

(fact "check board initialization"
  (((init-board) 5) 5) => :king
  (((init-board) 0) 5) => :black
  (((init-board) 5) 6) => :white
  (((init-board) 0) 0) => :castle
  (((init-board) 0) 10) => :castle)
