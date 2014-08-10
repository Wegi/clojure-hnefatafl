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

(defn update []
  (if @won
    (reset! message (str @won " won the game"))
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

(defn draw []
  (q/background 0xaa)  ;set bg to light gray
  (dorun (for [x (range 11)
               y (range 11)
               :let [piece (get-in @board [x y])]]
           (do
             (q/stroke 0xff 0xff 0xff)
             (cond
                (= piece :black)  (q/fill 0x00 0x00 0x00)
                (= piece :white)  (q/fill 0xff 0x99 0xff)
                (= piece :king)   (q/fill 0xe8 0x34 0x25)
                (= piece :castle) (q/fill 0x38 0xb4 0x8b)
                (= piece :throne) (q/fill 0xed 0xd3 0x09)
                (= piece :empty)  (q/fill 0xe3 0xe2 0xdd))
             (q/rect (* x 50) (* y 50) 50 50))))
  (q/fill 0x00)
  (q/text @message 20 560)
  (q/stroke 0xff 0xb2 0x25)  ;frame the chosen one
  (q/no-fill)
  (if-not (empty? @from)
    (q/rect (* (@from 0) 50) (* (@from 1) 50) 50 50))
  )

(defn mouse-pressed []
  (let [coords [(quot (q/mouse-x) 50) (quot (q/mouse-y) 50)]]
    (if (empty? @from)
      (reset! from coords)
      (reset! to coords))))

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
      :setup (fn [] (q/smooth) (q/no-stroke) (q/frame-rate 60))
      :draw (fn [] (update) (draw))
      :mouse-clicked mouse-pressed)))
