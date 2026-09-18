(ns clojure-warrior.test.play
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.api :as api]
    [clojure-warrior.play :as play]
    [clojure-warrior.units :as units]))

(defn message-texts [state]
  (keep :message/text (:state/messages state)))

(def init-state
  {:state/board [[{:unit/type :unit.type/warrior
                   :unit/health 10.0
                   :unit/direction :direction/east}
                  {:unit/type :unit.type/floor}]]
   :state/messages []
   :state/tick 0
   :state/turn 0})

(deftest play-turn
  (testing "play-turn"
    (let [users-code (fn [board]
                       [:action/walk :direction/forward])
          end-state (last (play/play-turn init-state users-code))]
      (is (= ["You walk forward"] (message-texts end-state)))
      (is (= 1 (:state/tick end-state)))
      (is (= 1 (:state/turn end-state)))
      (is (= :unit.type/floor (get-in end-state [:state/board 0 0 :unit/type])))
      (is (= :unit.type/warrior (get-in end-state [:state/board 0 1 :unit/type])))))

  (testing "input, say messages and output are added to the game report"
    (let [users-code (fn [board]
                       (api/say {:health 10.0})
                       [:action/walk :direction/forward])
          end-state (last (play/play-turn init-state users-code))]
      (is (= [{:message/type :message.type/input
               :message/board (play/get-public-state init-state)
               :message/turn 1}
              {:message/type :message.type/say
               :message/text "{:health 10.0}"
               :message/turn 1}
              {:message/type :message.type/warrior-action
               :message/action [:action/walk :direction/forward]
               :message/turn 1}
              {:message/type :message.type/system
               :message/text "You walk forward"
               :message/turn 1}]
             (:state/messages end-state)))))

  (testing "bot throws"
    (let [users-code (fn [board]
                       (api/say :before-boom)
                       (throw (ex-info "boom" {})))
          states (play/play-turn init-state users-code)
          end-state (last states)]
      (is (= 1 (count states)))
      (is (= true (:state/game-over? end-state)))
      (is (= 1 (:state/tick end-state)))
      (is (= 1 (:state/turn end-state)))
      (is (= (:state/board init-state) (:state/board end-state)))
      (is (= [{:message/type :message.type/input
               :message/board (play/get-public-state init-state)
               :message/turn 1}
              {:message/type :message.type/say
               :message/text ":before-boom"
               :message/turn 1}
              {:message/type :message.type/error
               :message/text "Your bot threw an error: boom"
               :message/turn 1}]
             (:state/messages end-state)))))

  (testing "bot returns an action without a direction"
    (let [users-code (fn [board]
                       [:action/walk])
          end-state (last (play/play-turn init-state users-code))
          [_input output error] (:state/messages end-state)]
      (is (= true (:state/game-over? end-state)))
      (is (= {:message/type :message.type/warrior-action
              :message/action [:action/walk]
              :message/turn 1}
             output))
      (is (= :message.type/error (:message/type error)))
      (is (re-find #"^Invalid action \[:action/walk\]: "
                   (:message/text error)))))

  (testing "bot uses an action that the level does not allow"
    (let [users-code (fn [board]
                       [:action/attack :direction/forward])
          end-state (last (play/play-turn (assoc init-state
                                            :state/abilities #{:action/walk})
                                          users-code))]
      (is (= true (:state/game-over? end-state)))
      (is (re-find #"is not available on this level"
                   (:message/text (last (:state/messages end-state)))))))

  (testing "bot returns something that is not an action vector"
    (doseq [action [nil :action/walk "walk" [:action/fly :direction/forward]]]
      (let [users-code (fn [board]
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
  (is (some? (play/action-error-text [:action/walk :direction/forward :extra])))

  (testing "abilities"
    (is (nil? (play/action-error-text [:action/walk :direction/forward]
                                      #{:action/walk})))
    (is (some? (play/action-error-text [:action/attack :direction/forward]
                                       #{:action/walk})))
    (is (some? (play/action-error-text [:action/rest]
                                       #{:action/walk})))))

(deftest start-level
  (testing "start-level"
    (let [level {:level/id 1
                 :level/board [[:*> nil nil :__]]}
          user-code (fn [board]
                      [:action/walk :direction/forward])
          end-state (last (play/start-level level user-code))]
      (is (= {:message/type :message.type/level-start
              :message/level level
              :message/turn 0}
             (first (:state/messages end-state))))
      (is (= ["You walk forward"
              "You walk forward"
              "You walk forward and up the stairs"]
             (message-texts end-state)))
      (is (= 3 (:state/turn end-state)))))

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
             (message-texts end-state))))))

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
        (is (= [[:message.type/system 0]
                [:message.type/level-start 0]
                [:message.type/input 1]
                [:message.type/warrior-action 1]
                [:message.type/system 1]
                [:message.type/level-start 1]
                [:message.type/input 2]
                [:message.type/warrior-action 2]
                [:message.type/system 2]
                [:message.type/system 2]]
               (map (juxt :message/type :message/turn) (:state/messages end-state))))
        (is (= 2 (:state/turn end-state)))
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
