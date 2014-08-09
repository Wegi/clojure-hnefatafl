(ns hnefatafl.core
  (:gen-class)
  (:require [clojure.string]
            [clojure.set]
            [clansi.core :as clansi]))

(defn init-board
  "Initialize the board with the start configuration."
  []
  [[:castle :empty :empty :black :black :black :black :black :empty :empty :castle]
   [:empty :empty :empty :empty :empty :black :empty :empty :empty :empty :empty]
   [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
   [:black :empty :empty :empty :empty :white :empty :empty :empty :empty :black]
   [:black :empty :empty :empty :white :white :white :empty :empty :empty :black]
   [:black :black :empty :white :white :king  :white :white :empty :black :black]
   [:black :empty :empty :empty :white :white :white :empty :empty :empty :black]
   [:black :empty :empty :empty :empty :white :empty :empty :empty :empty :black]
   [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
   [:empty :empty :empty :empty :empty :black :empty :empty :empty :empty :empty]
   [:castle :empty :empty :black :black :black :black :black :empty :empty :castle]])

(defn field-free?
  "Check if a given field is free"
  [board coord]
  (let [field (get-in board coord)]
    (or (= field :empty)
        (= field :throne)
        (= field :castle))))

(defn my-range
  "Range Function which automatically applies steps of 1 or -1.
Start exclusive goal inclusive."
  [start goal]
  (if (> 0 (- start goal))
    (range (inc start) (inc goal) 1)
    (range (dec start) (dec goal) -1)))

(defn way-clear?
  "Test if you can move a certain way."
  [board [f-x f-y :as from] [t-x t-y :as to]]
  (let [hor (map (fn [a] [a f-y]) (my-range f-x t-x))
        vert (map (fn [a] [f-x a]) (my-range f-y t-y))
        hormap (map (partial field-free? board) hor)
        vertmap (map (partial field-free? board) vert)]
    (not (or (some false? hormap) (some false? vertmap)))))

(defn valid-move
  "Check whether a move on a normal 11x11 Fetlar Hnefatafl is allowed"
  [[from-x from-y :as from] [to-x to-y :as to] piece board]
  (let [forbidden-set #{[0 0] [5 5] [0 10] [10 0] [10 10]}
        destination (get-in board to)]
    (if (or (or (< 10 from-x) (< 10 from-y) (< 10 to-x) (< 10 to-y))
            (or (> 0 from-y) (> 0 from-x) (> 0 to-x) (> 0 to-y))
            (and (not (= piece :king)) (contains? forbidden-set to))
            (and (not (= from-x to-x)) (not (= from-y to-y)))
            (not (field-free? board to))
            (not (way-clear? board from to)))
      false
      true
      )))

(defn new-board
  "Create a new board where one move is made."
  [board from to]
  (let [new-place (if (= from [5 5]) :throne :empty)
        new-place2 (if (contains? #{[0 0] [10 0] [0 10] [10 10]} from) :castle :empty)
        moved (get-in board from)]
    (assoc-in (assoc-in board to moved) from new-place2)))

(defn check-kill
  "Check one single postion and return updated board."
  [board [x y :as pos]]
  (let [piece (get-in board pos)
        enemies (if (= piece :black)
                  [:castle :throne :king :white]
                  [:castle :throne :black])
        checkpos [(get-in board [x (dec y)])
                  (get-in board [x (inc y)])
                  (get-in board [(dec x) y])
                  (get-in board [(inc x) y])]
        truecount (count ((group-by #(if (some #{%} enemies) true false) checkpos) true))]
    (if (and (= piece :king) (>= truecount 4))
      (new-board board pos pos)     ;kill position
      (if (and (not (= piece :king)) (>= truecount 2))
        (new-board board pos pos) ;kill position
        board))))

(defn won?
  "Test whether a player has won. Returns the winning player or nil if nobody wins."
  [board]
  (let [castles (set [(get-in board [0 0])
                      (get-in board [0 10])
                      (get-in board [10 0])
                      (get-in board [10 10])])
        lines (map #(set %) board)
        all-figures (apply clojure.set/union lines)]
    (if (contains? castles :king)
      :white-player
      (if (not (contains? all-figures :king))
        :black-player
        nil))))

(defn check-kills
  "Check a moved stone for kills and remove killed enemies. Returns checked board."
  [board [x y :as newpos]]
  (let [up (check-kill board [x (dec y)])
        right (check-kill up [(inc x) y])
        down (check-kill right [x (inc y)])
        left (check-kill down [(dec x) y])]
    left))

(defn move
  "Move a figure on a given board. (move board player from-coord to-coord)"
  [board player from-coord to-coord]
  (let [piece (get-in board from-coord)]
    (if (valid-move from-coord to-coord piece board)
      (case player
        :white-player (if (or (= piece :white) (= piece :king))
                        (let [after-move (new-board board from-coord to-coord)]
                          (check-kills after-move to-coord))
                        :wrong-piece)
        :black-player (if (= piece :black)
                        (let [after-move (new-board board from-coord to-coord)]
                          (check-kills after-move to-coord))
                        :wrong-piece)
        :not-a-player)
      :move-forbidden)))

(defn transpose [m]
  (apply mapv vector m))

(defn next-player [current]
  (if (= current :black-player)
    :white-player
    :black-player))

(defn convert-piece [piece]
  (case piece
    :black (clansi/style "B"  :blue :underline)
    :white (clansi/style "W" :white :underline)
    :king  (clansi/style "K" :magenta :underline)
    :castle (clansi/style "C" :green :underline)
    :throne (clansi/style "T" :red :underline)
    :empty "_"
    "??"))

(defn prepare-print [board]
  (mapv #(mapv convert-piece %) (transpose board)))

(defn -main
  "Main Loop"
  [& args]
  (loop [won false
         player :black-player
         board (init-board)]
    (if won
      (println "tfl> " won " won the Game")
      (do
        (doall (map println (prepare-print board)))
        (println "<------------------------------>")
        (println "tfl> " player " make your move (xfrom yfrom xto yto)")
        (let [move-input (read-line)
              split (clojure.string/split move-input #" ")
              from [(read-string (split 0)) (read-string (split 1))]
              to [(read-string (split 2)) (read-string (split 3))]
              result (move board player from to)]
          (println split)
          (if (contains? #{:wrong-piece :move-forbidden :not-a-player} result)
            (do
              (println "tfl> Error: " result)
              (recur false player board))
            ;; Move was made correctly
            (recur (won? result) (next-player player) result)))))))

;;TODO: Rand Schlagen
