(ns clojure-warrior.test.play
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.api :as api]
    [clojure-warrior.levels :as levels]
    [clojure-warrior.play :as play]
    [clojure-warrior.units :as units]))

(defn attack-or-walk [board]
  (if (:unit/enemy? (api/feel board :direction/forward))
    [:action/attack :direction/forward]
    [:action/walk :direction/forward]))

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
        (is (re-find #"must be a vector starting with"
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
              "You walk forward and up the stairs"
              "Level Score: 0"
              "Time Bonus: 0"
              "Clear Bonus: 0"
              "Total Score: 0 + 0 = 0"]
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
                [:message.type/system 1]
                [:message.type/system 1]
                [:message.type/system 1]
                [:message.type/system 1]
                [:message.type/level-score 1]
                [:message.type/level-start 1]
                [:message.type/input 2]
                [:message.type/warrior-action 2]
                [:message.type/system 2]
                [:message.type/system 2]
                [:message.type/system 2]
                [:message.type/system 2]
                [:message.type/system 2]
                [:message.type/level-score 2]
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
        (is (= 0 (:state/score end-state)))
        (is (= "You are dead. Game over."
               (:message/text (last (:state/messages end-state)))))))

    (testing "score accumulates across levels"
      (let [levels [{:level/id 1
                     :level/time-bonus 5
                     :level/board [[:*> :__]]}
                    {:level/id 2
                     :level/time-bonus 5
                     :level/board [[:*> :<s :__]]}]
            end-state (last (play/play-levels levels attack-or-walk))]
        ;; level 1: 1 turn -> time bonus 4, clear bonus round(0.8) = 1
        ;; level 2: 5 turns -> 12 points, time bonus 0, clear bonus round(2.4) = 2
        (is (= 19 (:state/score end-state)))
        (is (= ["Total Score: 0 + 5 = 5"
                "Total Score: 5 + 14 = 19"]
               (filter (fn [text]
                         (re-find #"^Total Score" text))
                       (message-texts end-state))))))

    (testing "check-abilities? flag"
      (let [levels [{:level/id 1
                     :level/abilities #{:action/walk}
                     :level/board [[:*> :<s :__]]}
                    {:level/id 2
                     :level/abilities #{:action/walk}
                     :level/board [[:*> :<s :__]]}]]
        (testing "checks abilities by default"
          (let [end-state (last (play/play-levels levels attack-or-walk))]
            (is (= true (:state/game-over? end-state)))
            (is (re-find #"is not available on this level"
                         (:message/text (last (:state/messages end-state)))))))
        (testing "skips ability check on every level when disabled"
          (let [end-state (last (play/play-levels levels attack-or-walk
                                                  {:check-abilities? false}))]
            (is (nil? (:state/game-over? end-state)))
            (is (= "You have reached the top of the tower"
                   (:message/text (last (:state/messages end-state)))))))))))

(deftest level-score
  (testing "passing a level tallies points, time bonus and clear bonus"
    (let [level {:level/id 1
                 :level/time-bonus 10
                 :level/board [[:*> :<s :__]]}
          end-state (last (play/start-level level attack-or-walk))]
      ;; 3 attacks + 2 walks
      (is (= 5 (:state/tick end-state)))
      (is (= 12 (:state/level-points end-state)))
      (is (= ["Level Score: 12"
              "Time Bonus: 5"
              "Clear Bonus: 3"
              "Total Score: 0 + 20 = 20"]
             (take-last 4 (message-texts end-state))))
      (is (= {:message/type :message.type/level-score
              :message/score {:score/points 12
                              :score/time-bonus 5
                              :score/clear-bonus 3
                              :score/total 20}
              :message/turn 5}
             (last (:state/messages end-state))))
      (is (= 20 (:state/score end-state)))))

  (testing "no clear bonus when a captive is left behind"
    (let [level {:level/id 1
                 :level/time-bonus 3
                 :level/board [[:*> :__ :<C]]}
          end-state (last (play/start-level level attack-or-walk))]
      (is (= ["Level Score: 0"
              "Time Bonus: 2"
              "Total Score: 0 + 2 = 2"]
             (take-last 3 (message-texts end-state))))
      (is (= 0 (get-in (last (:state/messages end-state)) [:message/score :score/clear-bonus])))
      (is (= 2 (:state/score end-state)))))

  (testing "time bonus does not go below zero"
    (let [level {:level/id 1
                 :level/time-bonus 1
                 :level/board [[:*> nil nil :__]]}
          end-state (last (play/start-level level attack-or-walk))]
      (is (= 3 (:state/tick end-state)))
      (is (= 0 (get-in (last (:state/messages end-state)) [:message/score :score/time-bonus])))
      (is (= 0 (:state/score end-state)))))

  (testing "no score when the warrior dies"
    (let [level {:level/id 1
                 :level/time-bonus 10
                 :level/board [[:*> :<w :__]]}
          end-state (last (play/start-level level (fn [board]
                                                    [:action/walk :direction/forward])))]
      (is (= true (:state/game-over? end-state)))
      (is (= 0 (:state/score end-state)))
      (is (empty? (filter (fn [message]
                            (= :message.type/level-score (:message/type message)))
                          (:state/messages end-state))))))

  (testing "matches the ace scores of the real levels"
    (let [history (play/play-levels (take 2 levels/levels) attack-or-walk)
          level-scores (->> history
                            last
                            :state/messages
                            (filter (fn [message]
                                      (= :message.type/level-score (:message/type message))))
                            (map (fn [message]
                                   (get-in message [:message/score :score/total]))))]
      (is (= (map :level/ace-score (take 2 levels/levels))
             level-scores))
      (is (= 36 (:state/score (last history)))))))

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
