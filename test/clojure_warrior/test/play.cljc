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
             (:state/messages end-state)))))

  (testing "bot throws"
    (let [init-state {:state/board [[{:unit/type :unit.type/warrior
                                      :unit/health 10.0
                                      :unit/direction :direction/east}
                                     {:unit/type :unit.type/floor}]]
                      :state/messages []
                      :state/tick 0}
          users-code (fn [board]
                       (api/say :before-boom)
                       (throw (ex-info "boom" {})))
          states (play/play-turn init-state users-code)
          end-state (last states)]
      (is (= 1 (count states)))
      (is (= true (:state/game-over? end-state)))
      (is (= 1 (:state/tick end-state)))
      (is (= (:state/board init-state) (:state/board end-state)))
      (is (= [{:message/type :message.type/say
               :message/text ":before-boom"}
              {:message/type :message.type/error
               :message/text "Your bot threw an error: boom"}]
             (:state/messages end-state)))))

  (testing "bot returns an action without a direction"
    (let [init-state {:state/board [[{:unit/type :unit.type/warrior
                                      :unit/health 10.0
                                      :unit/direction :direction/east}
                                     {:unit/type :unit.type/floor}]]
                      :state/messages []
                      :state/tick 0}
          users-code (fn [board]
                       [:action/walk])
          end-state (last (play/play-turn init-state users-code))]
      (is (= true (:state/game-over? end-state)))
      (is (= :message.type/error
             (:message/type (last (:state/messages end-state)))))
      (is (re-find #"^Invalid action \[:action/walk\]: "
                   (:message/text (last (:state/messages end-state)))))))

  (testing "bot returns something that is not an action vector"
    (doseq [action [nil :action/walk "walk" [:action/fly :direction/forward]]]
      (let [init-state {:state/board [[{:unit/type :unit.type/warrior
                                        :unit/health 10.0
                                        :unit/direction :direction/east}
                                       {:unit/type :unit.type/floor}]]
                        :state/messages []
                        :state/tick 0}
            users-code (fn [board]
                         action)
            end-state (last (play/play-turn init-state users-code))]
        (is (= true (:state/game-over? end-state)))
        (is (re-find #"must be a vector starting with one of"
                     (:message/text (last (:state/messages end-state)))))))))

(deftest action-error-text
  (is (nil? (play/action-error-text [:action/rest])))
  (is (nil? (play/action-error-text [:action/pivot])))
  (is (nil? (play/action-error-text [:action/walk :direction/forward])))
  (is (nil? (play/action-error-text [:action/attack :direction/backward])))
  (is (nil? (play/action-error-text [:action/shoot :direction/forward])))
  (is (nil? (play/action-error-text [:action/rescue :direction/backward])))
  (is (some? (play/action-error-text [:action/rest :direction/forward])))
  (is (some? (play/action-error-text [:action/walk :direction/north])))
  (is (some? (play/action-error-text [:action/walk :direction/forward :extra]))))

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
