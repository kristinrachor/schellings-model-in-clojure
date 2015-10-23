(ns schellings-model-in-clojure.model)

(def default-similarity 0.7)
(def default-balance 0.5)
(def default-empty 0.1)

; Atoms that reflect the state of the GUI sliders
; These aren't good names (they probably shouldn't say
; "atom", for example) so feel free to make them better.
(def similarity-atom (atom default-similarity))
(def balance-atom (atom default-balance))
(def empty-atom (atom default-empty))

(def population (atom []))
(def positions (atom []))
(def emptyPositions (atom []))


(defn myOwnLoop [neighbors likeMe color]
  (if (= (count neighbors) 0)
    likeMe
    (if (= (deref (first neighbors)) color)
      (swap! likeMe inc)
    )
  )
  (if (> (count neighbors) 0)
    (myOwnLoop (rest neighbors) likeMe color)
  )
  )

;(defn swapColor [_ color]
  ;color
  ;)
;(defn actaullySwap [_ me]
  ;me
  ;)

;(defn swapPositions [emptyPos me randNum]
  ;(update emptyPos randNum actaullySwap me)
  ;)

;(defn switchEmptyPosition [emptyPos me randNum]
  ;(send ((deref (get emptyPos randNum)) :individual) swapColor (deref (me :individual)))
  ;(send (me :individual) swapColor "white")
  ;)


  (defn swap-returning-prev!
    "Similar to swap! except returns vector containing the previous and new values
    (def a (atom 0))
    (swap-returning-prev! a inc) ;=> [0 1]"
    [atom f & args]
    (loop []
      (let [old-val @atom
            new-val (apply f (cons old-val args))
            success? (compare-and-set! atom old-val new-val)]
        (if success?
          [old-val new-val]
          (recur)))))

(defn switchColor [_ color]
  (println "COLOOR: " color)
  color
)

(defn notGoodName [_ newEmpty]
    newEmpty
  )

(defn switchEmpties [empties newEmpty oldEmptyIndex]
    (println "in switch empties: " (count empties))
    (println "in switch empties old index" (int oldEmptyIndex))
    (assoc empties (int oldEmptyIndex) newEmpty)
  )

(defn movePosition [me likeMe]
  ;choose random spot in emptyPostions
  ;remove it from emptyPosition and swap colors
  ;add me onto emptyPositions
  (let [randNum (Math/floor (rand (count (deref emptyPositions))))
        oldColor (deref ((deref me) :individual))]
  (println "I am not happy! " randNum)
  (send ((deref me) :individual) switchColor :white)
  (send ((deref (nth ((swap-returning-prev! emptyPositions switchEmpties me randNum) 0) (int randNum))) :individual) switchColor oldColor)
  ;(send emptyPositions switchEmptyPosition me randNum)
  ;(send emptyPositions swapPositions me randNum)
))

(defn amIHappy [me]
  (println "amihappy")
  (let [totalNeighbors (count ((deref me) :neighbors))
        likeMe (atom 0)
        color (deref ((deref me) :individual))]
  (if (> totalNeighbors 2)
    (myOwnLoop ((deref me) :neighbors) likeMe color)
  )
  (if (> totalNeighbors 2) (if (< (/ (deref likeMe) totalNeighbors) (deref similarity-atom)) (movePosition me likeMe) (println "I am happy!")))
    me))

(defn handle-neighbor-change
  "Called when the state of a neighboring position changes.
   The first argument will be the position atom that is being
   notified of the change, and the 4th and 5th arguments are
   the old and new states of the neighbor respectively. You
   might be able to use those to just directly update the red/blue
   counts for this position without requiring that it go look
   at its neighbors."
  [me key neighbor old-state new-state]
  ; You'll obviously want to replace this with some code that actually
  ; does something useful :-)
  ; If your positions contain individuals as agents, this is
  ; probably where you want to call send to handle whatever
  ; needs to be done. Otherwise everything will end up happening in
  ; the main thread.
  ;(println (str "Color " (if (deref me) ((deref me) :neighbors))))
  (println "There was a change")
  (if (not= :white (deref ((deref me) :individual))) (amIHappy me)))


;; You may be able to leave this alone, but feel free to change it
;; if you decide to structure your solution differently.
(defn make-position
  "Create a position atom that contains an individual agent, or nil
  if there's no individual there."
  []
  (if (< (rand) @empty-atom)
    (let [individual (agent :white)
          position (atom {:individual individual :neighbors (list)})]
      ; I need to have all the individuals together in
      ; a collection so I can `send` them all a "message"
      ; when, e.g., we hit the "Start" button.
      (swap! population conj individual)
      ; I'm not sure if I need all the positions, actually,
      ; but I found that useful for debugging.
      (swap! positions conj position)
      (swap! emptyPositions conj position)
      position)

    (let [color (if (< (rand) @balance-atom) :red :blue)
          individual (agent color)
          position (atom {:individual individual :neighbors (list)})]
      ; I need to have all the individuals together in
      ; a collection so I can `send` them all a "message"
      ; when, e.g., we hit the "Start" button.
      (swap! population conj individual)
      ; I'm not sure if I need all the positions, actually,
      ; but I found that useful for debugging.
      (swap! positions conj position)
      position)))

(defn get-individual-by-position
  "This will get an individual from populations and return it."
  [position]
  (let [x position]
     (deref (x :individual))))

(defn extract-color
  "Takes an individual agent and returns the color of the individual
   at that position. You'll need to implement this so that it returns
   desired color, or 'white' if there's no individual there."
  [position]
  ; This returns a totally random color so it should be quite
  ; obvious if you haven't dealt with this. You can specify colors
  ; with things like strings ("blue") or keywords (:red).
  (seesaw.color/color (get-individual-by-position position))
  )


