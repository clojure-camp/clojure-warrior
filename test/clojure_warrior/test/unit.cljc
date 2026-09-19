(ns clojure-warrior.test.unit
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.unit :as unit]
    [clojure-warrior.units :as units]))

(defn system-messages [& texts]
  (mapv (fn [text]
          {:message/type :message.type/system
           :message/text text})
        texts))

(def floor
  (:unit.type/floor units/reference))

(deftest take-warrior-action
  (testing "walk"
    (testing "can walk forward when open space"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/direction :direction/east}
                                  {:unit/type :unit.type/floor}]]
                   :state/messages []}
            action [:action/walk :direction/forward]
            expected-state {:state/board [[floor
                                           {:unit/type :unit.type/warrior
                                            :unit/direction :direction/east}]]
                            :state/messages (system-messages "You walk forward")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "can walk onto stairs"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/direction :direction/east}
                                  {:unit/type :unit.type/stairs}]]
                   :state/messages []}
            action [:action/walk :direction/forward]
            expected-state {:state/board [[floor
                                           {:unit/type :unit.type/warrior
                                            :unit/at-stairs true
                                            :unit/direction :direction/east}]]
                            :state/messages (system-messages "You walk forward and up the stairs")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "can walk backward when open space"
      (let [state {:state/board [[{:unit/type :unit.type/floor}
                                  {:unit/type :unit.type/warrior
                                   :unit/direction :direction/east}]]
                   :state/messages []}
            action [:action/walk :direction/backward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/direction :direction/east}
                                           floor]]
                            :state/messages (system-messages "You walk backward")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "can walk forward when open space (and facing west)"
      (let [state {:state/board [[{:unit/type :unit.type/floor}
                                  {:unit/type :unit.type/warrior
                                   :unit/direction :direction/west}]]
                   :state/messages []}
            action [:action/walk :direction/forward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/direction :direction/west}
                                           floor]]
                            :state/messages (system-messages "You walk forward")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "cannot walk forward when not open space"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/direction :direction/east}
                                  {:unit/type :unit.type/wall}]]
                   :state/messages []}
            action [:action/walk :direction/forward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/direction :direction/east}
                                           {:unit/type :unit.type/wall}]]
                            :state/messages (system-messages "You walk forward and bump into a wall")}]
        (is (= expected-state (unit/take-warrior-action state action))))))

  (testing "pivot"
    (testing "turns warrior east->west"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/direction :direction/east}]]
                   :state/messages []}
            action [:action/pivot]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/direction :direction/west}]]
                            :state/messages (system-messages "You pivot and are now facing west")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "turns warrior west->east"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/direction :direction/west}]]
                   :state/messages []}
            action [:action/pivot]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/direction :direction/east}]]
                            :state/messages (system-messages "You pivot and are now facing east")}]
        (is (= expected-state (unit/take-warrior-action state action))))))

  (testing "rest"
    (testing "get back 10% of max health"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/max-health 20.0
                                   :unit/health 5.0}]]
                   :state/messages []}
            action [:action/rest]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/max-health 20.0
                                            :unit/health 7.0}]]
                            :state/messages (system-messages "You rest and receive 2.0 health from resting, up to 7.0 health")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "does not get more than max-health"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/max-health 20.0
                                   :unit/health 19.0}]]
                   :state/messages []}
            action [:action/rest]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/max-health 20.0
                                            :unit/health 20.0}]]
                            :state/messages (system-messages "You rest and receive 1.0 health from resting, up to 20.0 health")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "does not heal at max-health"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/max-health 20.0
                                   :unit/health 20.0}]]
                   :state/messages []}
            action [:action/rest]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/max-health 20.0
                                            :unit/health 20.0}]]
                            :state/messages (system-messages "You rest but are already fit as a fiddle")}]
        (is (= expected-state (unit/take-warrior-action state action))))))

  (testing "attack"
    (testing "can attack forward"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/attack-power 5.0
                                   :unit/direction :direction/east}
                                  {:unit/type :whatever
                                   :unit/health 10.0}]]
                   :state/messages []}
            action [:action/attack :direction/forward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/attack-power 5.0
                                            :unit/direction :direction/east}
                                           {:unit/type :whatever
                                            :unit/health 5.0}]]
                            :state/messages (system-messages "You attack forward and a whatever takes 5.0 damage, and has 5.0 health left")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "can kill a unit, earning its max health as points"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/attack-power 5.0
                                   :unit/direction :direction/east}
                                  {:unit/type :whatever
                                   :unit/max-health 12.0
                                   :unit/health 5.0}]]
                   :state/messages []}
            action [:action/attack :direction/forward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/attack-power 5.0
                                            :unit/direction :direction/east}
                                           {:unit/type :whatever
                                            :unit/max-health 12.0
                                            :unit/health 0.0}]]
                            :state/level-points 12
                            :state/messages (system-messages "You attack forward and a whatever takes 5.0 damage, and dies. You earn 12 points.")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "can attack backward (at 50% reduced strength)"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/attack-power 5.0
                                   :unit/direction :direction/west}
                                  {:unit/type :whatever
                                   :unit/health 10.0}]]
                   :state/messages []}
            action [:action/attack :direction/backward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/attack-power 5.0
                                            :unit/direction :direction/west}
                                           {:unit/type :whatever
                                            :unit/health 7.5}]]
                            :state/messages (system-messages "You attack backward and a whatever takes 2.5 damage, and has 7.5 health left")}]
        (is (= expected-state (unit/take-warrior-action state action)))))

    (testing "attacking object without health has no effect"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/attack-power 5.0
                                   :unit/direction :direction/west}
                                  {:unit/type :whatever}]]
                   :state/messages []}
            action [:action/attack :direction/forward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/attack-power 5.0
                                            :unit/direction :direction/west}
                                           {:unit/type :whatever}]]
                            :state/messages (system-messages "You attack forward but you hit nothing")}]
        (is (= expected-state (unit/take-warrior-action state action))))))

  (testing "shoot"
    (testing "damages first unit within 2 units ahead"
      (testing "can shoot forward (range 1)"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/shoot-power 3.0
                                     :unit/direction :direction/east}
                                    {:unit/type :whatever
                                     :unit/health 10.0}]]
                     :state/messages []}
              action [:action/shoot :direction/forward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/shoot-power 3.0
                                              :unit/direction :direction/east}
                                             {:unit/type :whatever
                                              :unit/health 7.0}]]
                              :state/messages (system-messages "You shoot forward and a whatever takes 3.0 damage, and has 7.0 health left")}]
          (is (= expected-state (unit/take-warrior-action state action)))))

      (testing "can shoot forward (range 2)"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/shoot-power 3.0
                                     :unit/direction :direction/east}
                                    {:unit/type :unit.type/floor}
                                    {:unit/type :whatever
                                     :unit/health 10.0}]]
                     :state/messages []}
              action [:action/shoot :direction/forward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/shoot-power 3.0
                                              :unit/direction :direction/east}
                                             {:unit/type :unit.type/floor}
                                             {:unit/type :whatever
                                              :unit/health 7.0}]]
                              :state/messages (system-messages "You shoot forward and a whatever takes 3.0 damage, and has 7.0 health left")}]
          (is (= expected-state (unit/take-warrior-action state action)))))

      (testing "when nothing is in range, no effect"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/shoot-power 3.0
                                     :unit/direction :direction/east}
                                    {:unit/type :unit.type/floor}
                                    {:unit/type :unit.type/floor}
                                    {:unit/type :whatever
                                     :unit/health 10.0}]]
                     :state/messages []}
              action [:action/shoot :direction/forward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/shoot-power 3.0
                                              :unit/direction :direction/east}
                                             {:unit/type :unit.type/floor}
                                             {:unit/type :unit.type/floor}
                                             {:unit/type :whatever
                                              :unit/health 10.0}]]
                              :state/messages (system-messages "You shoot forward but you hit nothing")}]
          (is (= expected-state (unit/take-warrior-action state action)))))

      (testing "can shoot backward"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/shoot-power 3.0
                                     :unit/direction :direction/west}
                                    {:unit/type :whatever
                                     :unit/health 10.0}]]
                     :state/messages []}
              action [:action/shoot :direction/backward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/shoot-power 3.0
                                              :unit/direction :direction/west}
                                             {:unit/type :whatever
                                              :unit/health 7.0}]]
                              :state/messages (system-messages "You shoot backward and a whatever takes 3.0 damage, and has 7.0 health left")}]
          (is (= expected-state (unit/take-warrior-action state action)))))

      (testing "can kill a unit, earning its max health as points"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/shoot-power 3.0
                                     :unit/direction :direction/east}
                                    {:unit/type :whatever
                                     :unit/max-health 3.0
                                     :unit/health 2.0}]]
                     :state/level-points 20
                     :state/messages []}
              action [:action/shoot :direction/forward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/shoot-power 3.0
                                              :unit/direction :direction/east}
                                             {:unit/type :whatever
                                              :unit/max-health 3.0
                                              :unit/health 0.0}]]
                              :state/level-points 23
                              :state/messages (system-messages "You shoot forward and a whatever takes 2.0 damage, and dies. You earn 3 points.")}]
          (is (= expected-state (unit/take-warrior-action state action)))))

      (testing "shooting object without health has no effect"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/shoot-power 5.0
                                     :unit/direction :direction/east}
                                    {:unit/type :whatever}]]
                     :state/messages []}
              action [:action/shoot :direction/forward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/shoot-power 5.0
                                              :unit/direction :direction/east}
                                             {:unit/type :whatever}]]
                              :state/messages (system-messages "You shoot forward but you hit nothing")}]
          (is (= expected-state (unit/take-warrior-action state action)))))))

  (testing "rescue"
    (testing "receives 20 points; captive is marked rescued"
      (testing "can rescue forward"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/direction :direction/east}
                                    {:unit/type :unit.type/captive}]]
                     :state/messages []}
              action [:action/rescue :direction/forward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/direction :direction/east}
                                             {:unit/type :unit.type/captive
                                              :unit/rescued? true}]]
                              :state/level-points 20
                              :state/messages (system-messages "You reach forward and unbind a captive. You earn 20 points.")}]
          (is (= expected-state (unit/take-warrior-action state action)))))

      (testing "can rescue backward"
        (let [state {:state/board [[{:unit/type :unit.type/warrior
                                     :unit/direction :direction/west}
                                    {:unit/type :unit.type/captive}]]
                     :state/level-points 5
                     :state/messages []}
              action [:action/rescue :direction/backward]
              expected-state {:state/board [[{:unit/type :unit.type/warrior
                                              :unit/direction :direction/west}
                                             {:unit/type :unit.type/captive
                                              :unit/rescued? true}]]
                              :state/level-points 25
                              :state/messages (system-messages "You reach backward and unbind a captive. You earn 20 points.")}]
          (is (= expected-state (unit/take-warrior-action state action))))))

    (testing "if not a captive, no effect"
      (let [state {:state/board [[{:unit/type :unit.type/warrior
                                   :unit/direction :direction/east}
                                  {:unit/type :whatever}]]
                   :state/messages []}
            action [:action/rescue :direction/forward]
            expected-state {:state/board [[{:unit/type :unit.type/warrior
                                            :unit/direction :direction/east}
                                           {:unit/type :whatever}]]
                            :state/messages (system-messages "You reach forward but there is no captive to rescue")}]
        (is (= expected-state (unit/take-warrior-action state action)))))))
