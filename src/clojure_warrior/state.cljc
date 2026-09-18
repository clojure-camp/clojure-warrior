(ns clojure-warrior.state)

; getters

(defn get-units
  "Returns list of units, with their positions"
  [board]
  (->> board
       (map-indexed
         (fn [y row]
           (map-indexed (fn [x unit]
                          (assoc unit :unit/position [x y])) row)))
       flatten))

(defn get-warrior [board]
  (->> board
       get-units
       (filter (fn [unit] (= :unit.type/warrior (:unit/type unit))))
       first))

(defn get-stairs [board]
  (->> board
       get-units
       (filter (fn [unit] (= :unit.type/stairs (:unit/type unit))))
       first))

(defn unit-at-position [board position]
  (->> board
       get-units
       (filter (fn [unit] (= position (:unit/position unit))))
       first))

(defn first-unit-in-range [board unit action-direction action-range]
  (let [net-direction (case [(:unit/direction unit) action-direction]
                        [:direction/east :direction/forward] :direction/east
                        [:direction/east :direction/backward] :direction/west
                        [:direction/west :direction/forward] :direction/west
                        [:direction/west :direction/backward] :direction/east)
        maybe-reverse (case net-direction
                        :direction/east identity
                        :direction/west reverse)]
    (->> board
         get-units
         maybe-reverse
         (drop-while (fn [other]
                       (not= (:unit/position other) (:unit/position unit))))
         (drop 1)
         (take action-range)
         (remove (fn [other]
                   (= (:unit/type other) :unit.type/floor)))
         first)))

(defn action-target-position [warrior action-direction]
  (case [(:unit/direction warrior) action-direction]
    [:direction/east :direction/forward] (update-in (:unit/position warrior) [0] inc)
    [:direction/west :direction/backward] (update-in (:unit/position warrior) [0] inc)
    [:direction/west :direction/forward] (update-in (:unit/position warrior) [0] dec)
    [:direction/east :direction/backward] (update-in (:unit/position warrior) [0] dec)))

; modifiers

(defn add-message [state message]
  (let [message (if (string? message)
                  {:message/type :message.type/system
                   :message/text message}
                  message)]
    (update state :state/messages conj
            (cond-> message
              (contains? state :state/turn) (assoc :message/turn (:state/turn state))))))

(defn set-at [state position value]
  (assoc-in state [:state/board (last position) (first position)] value))

(defn assoc-at [state position k value]
  (assoc-in state [:state/board (last position) (first position) k] value))

(defn update-at [state position k fn]
  (update-in state [:state/board (last position) (first position) k] fn))
