(ns clojure-warrior.extract
  (:require
    [clojure-warrior.units :as units]))

(defn extract-unit
  "Given space notation from level description,
  returns object type and direction"
  [notation]
  (if-not notation
    (:unit.type/floor units/reference)
    (let [chars (set (seq (name notation)))
          type (or (units/define-char->type (first chars))
                   (units/define-char->type (last chars)))
          direction (cond
                      (contains? chars \>) :direction/east
                      (contains? chars \<) :direction/west)
          health (:unit/max-health (units/reference type))]
      (as-> (units/reference type) unit
        (if direction (assoc unit :unit/direction direction) unit)
        (if health (assoc unit :unit/health health) unit)))))

(defn extract-board [board-description]
  (->> board-description
       (map (fn [row]
              (concat [:-] row [:-])))
       (map (fn [row]
              (vec (map (fn [space]
                          (extract-unit space))
                        row))))
       vec))

(defn generate-initial-level-state
  [level-description]
  {:state/messages [{:message/type :message.type/level-start
                     :message/level level-description
                     :message/turn 0}]
   :state/board (extract-board (:level/board level-description))
   :state/tick 0
   :state/turn 0})
