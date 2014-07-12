(ns hnefatafl.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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

(defn way-clear?
  "Test if you can move a certain way."
  [board [f-x f-y :as from] [t-x t-y :as to]]
  (let [hor (map (fn [a] [a f-y]) (range (inc f-x) (inc t-x)))
        vert (map (fn [a] [f-x a]) (range (inc f-y) (inc t-y)))
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
  (let [new-place (if (= from [5 5]) :throne :empty)]
    (assoc-in (assoc-in board to :white) from new-place)))

(defn move
  "Move a figure on a given board. (move board player from-coord to-coord)"
  [board player from-coord to-coord]
  (let [piece (get-in board from-coord)]
    (if (valid-move from-coord to-coord piece board)
      (case player
        :white-player (if (or (= piece :white) (= piece :king))
                        (new-board board from-coord to-coord)
                        :wrong-piece)
        :black-player (if (= piece :black)
                        (new-board board from-coord to-coord)
                        :wrong-piece)
        :not-a-player)
      :move-forbidden)))


