(ns schellings-model-in-clojure.view
  (:require [schellings-model-in-clojure.model :as model]
            [seesaw.core :as sc]
            [seesaw.graphics :as sg]
            [seesaw.color :as scolor]
            [seesaw.table :as st]
            [seesaw.tree :as stree]
            [seesaw.bind :as sb]))

(def number-of-individuals-on-a-side 30)
(def individual-rectangle-size 10)

(defn neighborhood [[x y]]
  "Take a coordinate pair and return a list of all the
   neighborhood coordinate pairs."
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [new-x (+ x dx)
              new-y (+ y dy)]
        :when (and (not= [new-x new-y] [x y])
                   (>= new-x 0)
                   (< new-x number-of-individuals-on-a-side)
                   (>= new-y 0)
                   (< new-y number-of-individuals-on-a-side))]
    [new-x new-y]))

(defn neighbors [position board-map]
  "Take a position atom and the board map and return all the
   neighbor position atoms."
  (map board-map (neighborhood position)))

(defn make-coords [number-of-individuals-on-a-side]
  "Generate a list of all the legal coordinates on this board."
  (for [x (range number-of-individuals-on-a-side)
        y (range number-of-individuals-on-a-side)]
    [x y]))

(defn make-board-map [coordinates]
  "Take a list of coordinate pairs and construct new position atoms,
   possibly containing individual agents, and create a map from coordinates
   to position atoms."
  (into {}
        (for [c coordinates]
          [c (model/make-position)])))

(defn make-tile-map
  "Take a list of coordinate pairs and construct new tiles (small canvases),
   and create a map from coordinates to those tiles."
  [coordinates]
  (into {}
        (for [c coordinates]
          [c (sc/canvas :background "white"
                        :size [individual-rectangle-size :by individual-rectangle-size])])))

(defn add-neighborhood-watchers [board-map]
  "Take the board map, and generate all the watchers between position
   atoms in adjacent coordinate positions."
  (doseq [coord (keys board-map)
          neighbor (neighbors coord board-map)
          :let [position (board-map coord)]]
    ; The key (the second argument) has to be unique for each watcher
    ; on a given ref. In this case the ref is the position, so we need
    ; key to be unique for each watcher on a given position; I'm using
    ; neighbor for that, since no position should have more than one
    ; watcher for a given neighbor.
    (swap! (board-map coord) assoc :neighbors (conj ((deref (board-map coord)) :neighbors) (if (deref neighbor) ((deref neighbor) :individual))))

    (add-watch position (if (deref neighbor) ((deref neighbor) :individual))
               (partial model/handle-neighbor-change neighbor))))

(defn bind-tiles [coordinates board-map tile-map]
  "Set up the bindings between every position atom and the
   corresponding tile (with the same coordinate key). If the
   atom at that position changes, then the color of the bound
   tile should change to the color of the new value of that atom."
  (doseq [c coordinates]
    (sb/bind (board-map c)
             (sb/transform model/extract-color)
             (sb/property (tile-map c) :background))))

(defn create-tile-array
  "Set the color of each tile to the color for the associated
   position atom. It also converts the list of legal coordinates
   into a list of lists of tiles to aid in constructing the view."
  [coordinates tile-map board-map]
  (for [row (partition-by first coordinates)]
    (for [coord row
          :let [color (model/extract-color (deref (board-map coord)))
                tile (tile-map coord)]]
      (sc/config! tile :background color))))

(defn build-board [number-of-individuals-on-a-side]
  "Construct all the position atoms, individual agents, and tiles,
   connecting watchers and bindings as necessary. Returns the
   tiles as a list of rows, each of which is a list of tiles."
  (let [coordinates (make-coords number-of-individuals-on-a-side)
        board-map (make-board-map coordinates)
        tile-map (make-tile-map coordinates)]
    (add-neighborhood-watchers board-map)
    (bind-tiles coordinates board-map tile-map)
    (create-tile-array coordinates tile-map board-map)))

(defn make-tile-grid
  "Convert the list of list of tiles into GUI components."
  [tile-array]
  (sc/vertical-panel
   :items (for [row tile-array]
            (sc/horizontal-panel :items row))))

(def reset-button
  (sc/button :text "Reset"))
(def start-stop-button
  (sc/button :text "Start"))

(def buttons
  (sc/vertical-panel
   :items [reset-button start-stop-button]))

(defn make-slider [slider-label initial-value slider-atom]
  (let [title-label (sc/label :text (str slider-label ": "))
        value-label (sc/label :text initial-value)
        slider (sc/slider :orientation :horizontal
                          :value (Math/round (* 100 initial-value))
                          :min 0
                          :max 100)]
    (sb/bind slider (sb/transform / 100.0) slider-atom)
    (sb/bind slider-atom (sb/transform #(Math/round (* 100.0 %))) value-label)
    (sc/vertical-panel
     :items [(sc/horizontal-panel :items [title-label value-label])
             slider])))

(def sliders
  (sc/vertical-panel
   :items (map (partial apply make-slider)
               [["Similarity tolerance" model/default-similarity model/similarity-atom]
                ["Relative proportions" model/default-balance model/balance-atom]
                ["Proportion empty" model/default-empty model/empty-atom]])))

(def controls
  (sc/horizontal-panel
   :items [buttons sliders]))

(defn make-window-content
  [tile-array]
  (sc/border-panel
   :center (make-tile-grid tile-array)
   :south controls))

(def title "Schelling's model of segregation")
(def window-size 800)

(def main-window
  (let [tile-array (build-board number-of-individuals-on-a-side)]
    (sc/frame :title title
              :width (:width window-size)
              :height (:height window-size)
              :content (make-window-content tile-array))))
