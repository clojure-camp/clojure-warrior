(ns clojure-warrior.test.play
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.api :as api]
    [clojure-warrior.play :as play]
    [clojure-warrior.units :as units]))

(defn message-texts [state]
  (map :message/text (:state/messages state)))

(deftest play-turn
  (testing "play-turn"
    (let [init-state {:state/board [[{:unit/type :unit.type/warrior
                                      :unit/health 10.0
                                      :unit/direction :direction/east}
                                     {:unit/type :unit.type/floor}]]
                      :state/messages []
                      :state/tick 0}
          users-code (fn [board]
                       [:action/walk :direction/forward])
          end-state (last (play/play-turn init-state users-code))]
      (is (= ["You walk forward"] (message-texts end-state)))
      (is (= 1 (:state/tick end-state)))
      (is (= :unit.type/floor (get-in end-state [:state/board 0 0 :unit/type])))
      (is (= :unit.type/warrior (get-in end-state [:state/board 0 1 :unit/type])))))

  (testing "say messages are added to the game report"
    (let [init-state {:state/board [[{:unit/type :unit.type/warrior
                                      :unit/health 10.0
                                      :unit/direction :direction/east}
                                     {:unit/type :unit.type/floor}]]
                      :state/messages []
                      :state/tick 0}
          users-code (fn [board]
                       (api/say {:health 10.0})
                       [:action/walk :direction/forward])
          end-state (last (play/play-turn init-state users-code))]
      (is (= [{:message/type :message.type/say
               :message/text "{:health 10.0}"}
              {:message/type :message.type/system
               :message/text "You walk forward"}]
             (:state/messages end-state))))))

(deftest start-level
  (testing "start-level"
    (let [level {:level/id 1
                 :level/board [[:*> nil nil :__]]}
          user-code (fn [board]
                      [:action/walk :direction/forward])
          end-state (last (play/start-level level user-code))]
      (is (= {:message/type :message.type/level-start
              :message/level level}
             (first (:state/messages end-state))))
      (is (= ["You walk forward"
              "You walk forward"
              "You walk forward and up the stairs"]
             (rest (message-texts end-state))))))

  (testing "player death"
    (let [level {:level/id 1
                 :level/board [[:*> :<w]]}
          user-code (fn [board]
                      [:action/walk :direction/forward])
          end-state (last (play/start-level level user-code))]
      (is (= true (:state/game-over? end-state)))
      (is (= ["You walk forward and bump into a wizard"
              "A wizard shoots you and you lose 11.0 health, down to 9.0"
              "You walk forward and bump into a wizard"
              "A wizard shoots you and you lose 9.0 health, down to 0.0"
              "You are dead. Game over."]
             (rest (message-texts end-state)))))))

(deftest play-levels
  (testing "play-levels"
    (testing "win game"
      (let [levels [{:level/id 1
                     :level/board [[:*> :__]]}
                    {:level/id 2
                     :level/board [[:*> :__]]}]
            user-code (fn [board]
                        [:action/walk :direction/forward])
            end-state (last (play/play-levels levels user-code))]
        (is (= [:message.type/system :message.type/level-start :message.type/system :message.type/level-start :message.type/system :message.type/system]
               (map :message/type (:state/messages end-state))))
        (is (= "You have reached the top of the tower"
               (:message/text (last (:state/messages end-state)))))))

    (testing "lose game, due to death"
      (let [levels [{:level/id 1
                     :level/board [[:*> nil :<w :__]]}
                    {:level/id 2
                     :level/board [[:*> :__]]}]
            user-code (fn [board]
                        [:action/attack :direction/forward])
            end-state (last (play/play-levels levels user-code))]
        (is (= true (:state/game-over? end-state)))
        (is (= "You are dead. Game over."
               (:message/text (last (:state/messages end-state)))))))))

(deftest get-public-unit
  (let [private-unit (assoc (:unit.type/archer units/reference)
                            :unit/health 10.0
                            :unit/direction :direction/east)]
    (is (= {:unit/type :unit.type/archer
            :unit/enemy? true
            :unit/ranged? true
            :unit/health 10.0
            :unit/direction :direction/east}
           (play/get-public-unit private-unit)))))

(deftest get-public-state
  (testing "get-public-state"
    (let [state {:state/board [[{:unit/type :unit.type/warrior
                                 :unit/logic :secret
                                 :unit/health 10.0}]]}]
      (is (= [[{:unit/type :unit.type/warrior
                :unit/health 10.0}]]
             (play/get-public-state state))))))
