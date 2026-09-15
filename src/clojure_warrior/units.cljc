(ns clojure-warrior.units
  (:require
    [clojure-warrior.api :as api]))

(defn melee-unit-logic [board self]
  (cond
    (= :unit.type/warrior (:unit/type (api/feel-generic board self :direction/forward)))
    [:action/attack :direction/forward]
    (= :unit.type/warrior (:unit/type (api/feel-generic board self :direction/backward)))
    [:action/attack :direction/backward]
    :else
    nil))

(defn ranged-unit-logic [board self]
  (cond
    (= :unit.type/warrior (:unit/type (api/look-generic board self :direction/forward 2)))
    [:action/shoot :direction/forward]
    (= :unit.type/warrior (:unit/type (api/look-generic board self :direction/backward 2)))
    [:action/shoot :direction/backward]
    :else
    nil))

(def reference
  {:unit.type/captive
   {:unit/type :unit.type/captive
    :unit/captive? true
    :unit/max-health 1.0
    :unit/define-char \C
    :unit/display-char \C}

   :unit.type/archer
   {:unit/type :unit.type/archer
    :unit/enemy? true
    :unit/ranged? true
    :unit/define-char \a
    :unit/display-char \a
    :unit/shoot-power 3.0
    :unit/max-health 7.0
    :unit/logic ranged-unit-logic}

   :unit.type/sludge
   {:unit/type :unit.type/sludge
    :unit/enemy? true
    :unit/melee? true
    :unit/define-char \s
    :unit/display-char \s
    :unit/attack-power 3.0
    :unit/max-health 12.0
    :unit/logic melee-unit-logic}

   :unit.type/thick-sludge
   {:unit/type :unit.type/thick-sludge
    :unit/enemy? true
    :unit/melee? true
    :unit/define-char \S
    :unit/display-char \S
    :unit/attack-power 3.0
    :unit/max-health 24.0
    :unit/logic melee-unit-logic}

   :unit.type/wizard
   {:unit/type :unit.type/wizard
    :unit/enemy? true
    :unit/ranged? true
    :unit/define-char \w
    :unit/display-char \w
    :unit/shoot-power 11.0
    :unit/max-health 3.0
    :unit/logic ranged-unit-logic}

   :unit.type/warrior
   {:unit/type :unit.type/warrior
    :unit/ranged? true
    :unit/melee? true
    :unit/define-char \*
    :unit/display-char \@
    :unit/max-health 20.0
    :unit/attack-power 5.0
    :unit/shoot-power 3.0}

   :unit.type/wall
   {:unit/type :unit.type/wall
    :unit/environment? true
    :unit/define-char \-
    :unit/display-char \|}

   :unit.type/stairs
   {:unit/type :unit.type/stairs
    :unit/stairs? true
    :unit/empty? true
    :unit/environment? true
    :unit/define-char \_
    :unit/display-char \>}

   :unit.type/floor
   {:unit/type :unit.type/floor
    :unit/empty? true
    :unit/environment? true
    :unit/define-char nil
    :unit/display-char " "}})

(def define-char->type
  (reduce
    (fn [memo unit]
      (assoc memo (:unit/define-char unit) (:unit/type unit)))
    {}
    (vals reference)))
