(ns schellings-model-in-clojure.core
  (:require
   [seesaw.core :as sc]
   [schellings-model-in-clojure.model :as model]
   [schellings-model-in-clojure.view :as view]
   [schellings-model-in-clojure.controller :as controller])
  (:gen-class))

(defn show-gui []
  (sc/native!)
  (sc/show! (sc/pack! view/main-window))
  (controller/setup-listeners)
  )

(defn -main
  "Start up the simulation of Schelling's model"
  [& args]
  (show-gui)
  ;; You should remove everything from here down. I only
  ;; included this to "force" some sort of change in the
  ;; system so I could confirm that all my watching and
  ;; binding was wired up correctly.
  (let [f (first (deref model/positions))
        fi @f
        s (second (deref model/positions))
        si @s]
    (Thread/sleep 4000)
    (println (nth (deref model/positions) 0))
    (model/amIHappy (nth (deref model/positions) 0)))
    (Thread/sleep 2000)
    (println (nth (deref model/positions) 0))
  )
