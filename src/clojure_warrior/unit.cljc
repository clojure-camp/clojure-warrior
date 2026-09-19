(ns clojure-warrior.unit
  (:require
    [clojure-warrior.state :refer [get-warrior
                                   unit-at-position
                                   first-unit-in-range
                                   action-target-position
                                   add-message
                                   add-points
                                   set-at
                                   assoc-at]]
    [clojure-warrior.units :as units]))

(defn damage-unit [state target damage action-text]
  (let [target-new-health (max 0 (- (:unit/health target) damage))
        message-text (str action-text " and a " (name (:unit/type target)) " takes " damage " damage, ")]
    (if (< 0 target-new-health)
      (-> state
          (add-message (str message-text "and has " target-new-health " health left"))
          (assoc-at (:unit/position target) :unit/health target-new-health))
      (let [points (long (:unit/max-health target))]
        (-> state
            (add-message (str message-text "and dies. You earn " points " points."))
            (assoc-at (:unit/position target) :unit/health target-new-health)
            (add-points points))))))

(defmulti take-warrior-action
  "Returns new state after performing warrior action"
  (fn [_state action] (first action)))

(defmethod take-warrior-action :action/walk
  [state [_ direction]]
  (let [warrior (get-warrior (:state/board state))
        target-position (action-target-position warrior direction)
        target (unit-at-position (:state/board state) target-position)
        action-text (str "You walk " (name direction))]
    (as-> state $
      (cond
        (= :unit.type/floor (:unit/type target))
        (-> $
            (add-message action-text)
            (set-at (:unit/position warrior) (:unit.type/floor units/reference))
            (set-at (:unit/position target) (dissoc warrior :unit/position)))
        (= :unit.type/stairs (:unit/type target))
        (-> $
            (add-message (str action-text " and up the stairs"))
            (set-at (:unit/position warrior) (:unit.type/floor units/reference))
            (set-at (:unit/position target) (-> warrior
                                                (assoc :unit/at-stairs true)
                                                (dissoc :unit/position))))
        :else
        (add-message $ (str action-text " and bump into a " (name (:unit/type target))))))))

(defmethod take-warrior-action :action/pivot
  [state _]
  (let [warrior (get-warrior (:state/board state))
        new-direction (case (:unit/direction warrior)
                        :direction/east :direction/west
                        :direction/west :direction/east)]
    (-> state
        (add-message (str "You pivot and are now facing " (name new-direction)))
        (assoc-at (:unit/position warrior) :unit/direction new-direction))))

(defmethod take-warrior-action :action/rest
  [state _]
  (let [warrior (get-warrior (:state/board state))
        max-health (:unit/max-health warrior)
        health (:unit/health warrior)
        new-health (min max-health (+ health (* max-health 0.1)))
        health-delta (- new-health health)
        action-text "You rest"]
    (as-> state $
        (if (> health-delta 0)
          (add-message $ (str action-text " and receive " health-delta " health from resting, up to " new-health " health"))
          (add-message $ (str action-text " but are already fit as a fiddle")))
        (assoc-at $ (:unit/position warrior) :unit/health new-health))))

(defmethod take-warrior-action :action/attack
  [state [_ direction]]
  (let [warrior (get-warrior (:state/board state))
        target-position (action-target-position warrior direction)
        target (unit-at-position (:state/board state) target-position)
        power-multiplier (case direction
                           :direction/forward 1.0
                           :direction/backward 0.5)
        attack-power (* (:unit/attack-power warrior) power-multiplier)
        action-text (str "You attack " (name direction))]
    (if (and target (:unit/health target))
      (damage-unit state target (min attack-power (:unit/health target)) action-text)
      (add-message state (str action-text " but you hit nothing")))))

(defmethod take-warrior-action :action/shoot
  [state [_ direction]]
  (let [warrior (get-warrior (:state/board state))
        target (first-unit-in-range (:state/board state) warrior direction 2)
        attack-power (:unit/shoot-power warrior)
        action-text (str "You shoot " (name direction))]
    (if (and target (:unit/health target))
      (damage-unit state target (min attack-power (:unit/health target)) action-text)
      (add-message state (str action-text " but you hit nothing")))))

(defmethod take-warrior-action :action/rescue
  [state [_ direction]]
  (let [warrior (get-warrior (:state/board state))
        target (first-unit-in-range (:state/board state) warrior direction 1)
        action-text (str "You reach " (name direction))]
    (as-> state $
      (if (and target (= :unit.type/captive (:unit/type target)))
        (-> $
            (add-message (str action-text " and unbind a captive. You earn 20 points."))
            (assoc-at (:unit/position target) :unit/rescued? true)
            (add-points 20))
        (add-message $ (str action-text " but there is no captive to rescue"))))))
