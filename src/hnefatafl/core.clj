(ns hnefatafl.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def board
  )

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
