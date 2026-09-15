(ns clojure-warrior.display
  (:require
    [clojure.string :as string]))

(defn generate-display [state]
  (let [width (count (first (:state/board state)))
        line (string/join "" (repeat width "-"))]
    (string/join "\n"
      (concat [line]
              (->> state
                   :state/board
                   (map (fn [row]
                          (->> row
                               (map (fn [space]
                                      (:unit/display-char space)))
                               (string/join "")))))
              [line]))))
