(ns hnefatafl.ui
  (:gen-class)
  (:use [hnefatafl.core]
        [quil.core :as q]))

(def board (atom (init-board)))
(def won (atom false))
(def player (atom :black-player))
(def from (atom []))
(def to (atom []))
(def message (atom ":black-player it is your turn"))
(def viking (atom nil))
(def viking-white (atom nil))
(def king (atom nil))
(def tower (atom nil))
(def grass (atom nil))
(def throne (atom nil))

(defn update []
  (if @won
    (do
      (if (= @won "tied")
        (reset! message (str "The game is a tie"))
        (reset! message (str @won " won the game"))))
    (do
      (if (empty? @from)
        (reset! message (str @player " chose the piece to move"))
        (reset! message (str @player " where do you want to move to?")))
      (if (and (not (empty? @from)) (not (empty? @to)))
        (let [new-board (move @board @player @from @to)]
          (if (contains? #{:wrong-piece :move-forbidden :not-a-player} new-board)
            (reset! message (str "Error: " new-board ". Select a new move"))
            (do (if (= @player :black-player)
                  (reset! player :white-player)
                  (reset! player :black-player))
                (reset! board new-board)
                (reset! won (won? @board))))
          (reset! from [])
          (reset! to []))))))

(defn draw-button [x y xh yh text]
  (q/fill 0xff)
  (q/stroke 0x00)
  (q/rect x y xh yh)     ;tie button
  (q/fill 0x00)
  (q/text text (+ x 25) (+ y 15)))

(defn draw []
  (q/background 0xaa)  ;set bg to light gray
  (dorun (for [x (range 11)
               y (range 11)
               :let [piece (get-in @board [x y])
                     xx (* x 50)
                     yy (* y 50)]]
           (do
             (q/image @grass xx yy)
             (cond
                (= piece :black)  (q/image @viking xx yy)
                (= piece :white)  (q/image @viking-white xx yy)
                (= piece :king)   (q/image @king xx yy)
                (= piece :castle) (q/image @tower xx yy)
                (= piece :throne) (q/image @throne xx yy))
             (q/stroke 0xff 0xff 0xff)
             (q/no-fill)
             (q/rect xx yy 50 50))))
  (draw-button 400 560 100 20 "Tie Game")
  (q/text @message 20 560)   ;write the game state
  (q/stroke 0xff 0xb2 0x25)  ;frame the chosen one
  (q/no-fill)
  (if-not (empty? @from)
    (q/rect (* (@from 0) 50) (* (@from 1) 50) 50 50))
  )

(defn between-nums?
  [right left num]
  (if (and (<= right num) (<= num left))
    true
    false))

(defn pressed-button? [x y]
  (if (and (between-nums? 400 500 x) (between-nums? 560 580 y))
    :button-tie
    nil))

(defn mouse-pressed []
  (let [coords [(quot (q/mouse-x) 50) (quot (q/mouse-y) 50)]]
    (if (and (empty? @from) (< (coords 0) 11) (< (coords 1) 11))
      (reset! from coords)
      (reset! to coords)))
  (let [button (pressed-button? (q/mouse-x) (q/mouse-y))]
    (case button
      :button-tie (reset! won "tied")
      :else)))

(defn setup []
  (q/smooth)
  (q/no-stroke)
  (q/frame-rate 60)
  (reset! viking (q/load-image (clojure.java.io/resource "viking.png")))
  (reset! viking-white (q/load-image (clojure.java.io/resource "viking-white.png")))
  (reset! king (q/load-image  (clojure.java.io/resource "king.png")))
  (reset! tower (q/load-image (clojure.java.io/resource "tower.png")))
  (reset! grass (q/load-image  (clojure.java.io/resource "hay.jpg")))
  (reset! throne (q/load-image (clojure.java.io/resource "throne.png"))))

(defn -main
  "Main Loop"
  [& args]
  (if args
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
            (if (contains? #{:wrong-piece :move-forbidden :not-a-player} result)
              (do
                (println "tfl> Error: " result)
                (recur false player board))
              ;; Move was made correctly
              (recur (won? result) (next-player player) result))))))
    (q/defsketch hnefatafl
      :title "Clojure Hnefatafl (Fetlar Edition)"
      :size [550 600]
      :setup setup
      :draw (fn [] (update) (draw))
      :mouse-clicked mouse-pressed)))
