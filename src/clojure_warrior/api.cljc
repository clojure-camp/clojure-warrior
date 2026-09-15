(ns clojure-warrior.api
  (:require
    [clojure.string :as string]
    [clojure-warrior.state :as state]))

(def ^:dynamic *say-listener*
  ;; bound during a game to collect messages into the game report
  nil)

(defn say
  "Sends args to tap> and, during a game, to the game report"
  [& args]
  (when *say-listener*
    (*say-listener*
      (->> args
           (map (fn [arg]
                  (if (string? arg)
                    arg
                    (pr-str arg))))
           (string/join " "))))
  (tap> (if (= 1 (count args))
          (first args)
          (vec args)))
  nil)

(defn stairs
  "Returns the stairs"
  [board]
  (state/get-stairs board))

(defn warrior
  "Returns the warrior"
  [board]
  (state/get-warrior board))

(defn look-generic
  [board unit direction limit]
  (state/first-unit-in-range
    board
    unit
    direction
    limit))

(defn look
  "Returns the first non-empty space in given direction from the warrior"
  [board direction]
  (look-generic board (warrior board) direction 1000))

(defn feel-generic
  [board unit direction]
  (state/unit-at-position
    board
    (state/action-target-position
      unit
      direction)))

(defn feel
  "Return the space 1 unit in given direction from the warrior"
  [board direction]
  (feel-generic board (warrior board) direction))

(defn listen
  "Returns a list of all enemies and captives"
  [board]
  (->> (state/get-units board)
       (remove (fn [unit]
                 (contains? #{:unit.type/warrior :unit.type/floor :unit.type/wall :unit.type/stairs} (:unit/type unit))))))

(defn distance-to
  "Returns the number of steps from the warrior to a position"
  [board target-position]
  (let [warrior-position (:unit/position (warrior board))]
    (+ (abs (- (first target-position) (first warrior-position)))
       (abs (- (last target-position) (last warrior-position))))))

(defn inspect
  "Returns the space at the given position"
  [board target-position]
  (state/unit-at-position board target-position))
