(ns clojure-warrior.play
  (:require
    [malli.core :as m]
    [malli.error :as me]
    [clojure-warrior.api :as api]
    [clojure-warrior.extract :as extract]
    [clojure-warrior.state :refer [get-warrior
                                   add-message
                                   assoc-at]]
    [clojure-warrior.unit :refer [take-warrior-action]]
    [clojure-warrior.units :as units]))

(def Direction
  [:enum :direction/forward :direction/backward])

(def Action
  [:multi {:dispatch (fn [action]
                       (when (sequential? action)
                         (first action)))}
   [:action/walk [:tuple [:= :action/walk] Direction]]
   [:action/attack [:tuple [:= :action/attack] Direction]]
   [:action/shoot [:tuple [:= :action/shoot] Direction]]
   [:action/rescue [:tuple [:= :action/rescue] Direction]]
   [:action/rest [:tuple [:= :action/rest]]]
   [:action/pivot [:tuple [:= :action/pivot]]]
   [::m/default [:fn {:error/message "must be a vector starting with one of :action/walk :action/attack :action/shoot :action/rescue :action/rest :action/pivot"}
                 (constantly false)]]])

(defn action-error-text [action]
  (when-let [explanation (m/explain Action action)]
    (str "Invalid action " (pr-str action) ": "
         (pr-str (me/humanize explanation)))))

(defn end-with-error [state text]
  (-> state
      (add-message {:message/type :message.type/error
                    :message/text text})
      (assoc :state/game-over? true)))

(defn map-units [f board]
  (mapv (fn [row]
          (mapv f row)) board))

(defn get-public-unit [unit]
  (select-keys unit [:unit/type :unit/health :unit/direction
                     :unit/enemy? :unit/melee? :unit/ranged?
                     :unit/captive? :unit/empty? :unit/stairs?]))

(defn get-public-state [state]
  (->> state
       :state/board
       (map-units get-public-unit)))

(defn remove-rescued-captives [state]
  (update state :state/board
          (fn [board]
            (map-units (fn [unit]
                         (if (:unit/rescued? unit)
                           (:unit.type/floor units/reference)
                           unit))
                       board))))

(defn remove-dead-units [state]
  (update state :state/board
          (fn [board]
            (map-units (fn [unit]
                         (if (and
                               (contains? unit :unit/health)
                               (>= 0 (:unit/health unit)))
                           (:unit.type/floor units/reference)
                           unit))
                       board))))

(defn warrior-at-stairs? [state]
  (->> (:state/board state)
       flatten
       (map :unit/at-stairs)
       (some true?)))

(defn increment-tick [state]
  (update state :state/tick inc))

(defn check-warrior-dead [state]
  (if (= 0.0 (:unit/health (get-warrior (:state/board state))))
    (-> state
        (assoc :state/game-over? true)
        (add-message "You are dead. Game over."))
    state))

(defn check-warrior-stalled [state]
  (if (< 200 (:state/tick state))
    (-> state
        (assoc :state/game-over? true)
        (add-message "You have taken too long. Game over."))
    state))

(defmulti take-enemy-action
  (fn [_state _enemy action]
    (first action)))

(defmethod take-enemy-action :action/rest
  [state _enemy _]
  ; do nothing
  state)

(defmethod take-enemy-action :action/shoot
  [state enemy _]
  (let [warrior (get-warrior (:state/board state))
        strength (:unit/shoot-power enemy)
        new-health (max 0.0 (- (:unit/health warrior) strength))
        health-delta (- (:unit/health warrior) new-health)]
    (-> state
        (add-message {:message/type :message.type/enemy-action
                      :message/text (str "A " (name (:unit/type enemy)) " shoots you"
                                         " and you lose " health-delta " health, down to " new-health)})
        (assoc-at (:unit/position warrior) :unit/health new-health))))

(defmethod take-enemy-action :action/attack
  [state enemy _]
  (let [warrior (get-warrior (:state/board state))
        strength (:unit/attack-power enemy)
        new-health (max 0.0 (- (:unit/health warrior) strength))
        health-delta (- (:unit/health warrior) new-health)]
    (-> state
        (add-message {:message/type :message.type/enemy-action
                      :message/text (str "A " (name (:unit/type enemy)) " attacks you"
                                         " and you lose " health-delta " health, down to " new-health)})
        (assoc-at (:unit/position warrior) :unit/health new-health))))

(defn store-enemy-action [state enemy action]
  (let [[x y] (:unit/position enemy)]
    (assoc-in state [:state/board y x :unit/action] action)))

(defn take-npc-actions [state]
  (let [enemies (api/listen (:state/board state))]
    (reduce
      (fn [state enemy]
        (if (:unit/logic enemy)
          (if-let [action ((:unit/logic enemy) (:state/board state) enemy)]
            (-> state
                (store-enemy-action enemy action)
                (take-enemy-action enemy action))
            state)
          state))
      state
      enemies)))

(defn reset-npc-actions [state]
  (let [enemies (api/listen (:state/board state))]
    (reduce
      (fn [state enemy]
        (store-enemy-action state enemy nil))
      state
      enemies)))

(defn store-warrior-action [state action]
  (let [[x y] (:unit/position (get-warrior (:state/board state)))]
    (assoc-in state [:state/board y x :unit/action] action)))

(defn increment-turn [state]
  (update state :state/turn inc))

(defn add-turn-messages [state {:keys [input say-messages result]}]
  (as-> state $
    (add-message $ {:message/type :message.type/input
                    :message/board input})
    (reduce add-message $ say-messages)
    (if (contains? result :action)
      (add-message $ {:message/type :message.type/warrior-action
                      :message/action (:action result)})
      $)))

(defn play-turn [init-state users-code]
  (let [say-messages (atom [])
        input (get-public-state init-state)
        result (try
                 (binding [api/*say-listener*
                           (fn [text]
                             (swap! say-messages conj
                                    {:message/type :message.type/say
                                     :message/text text}))]
                   {:action (users-code input)})
                 (catch #?(:clj Exception :cljs :default) error
                   {:error (str "Your bot threw an error: "
                                (or (ex-message error)
                                    (str error)))}))
        error-text (or (:error result)
                       (action-error-text (:action result)))
        add-log-messages (fn [state]
                           (add-turn-messages state {:input input
                                                     :say-messages @say-messages
                                                     :result result}))]
    (if error-text
      [(-> init-state
           increment-tick
           increment-turn
           add-log-messages
           (end-with-error error-text))]
      (let [warrior-action (:action result)
            post-warrior-state (-> init-state
                                   increment-tick
                                   increment-turn
                                   (store-warrior-action warrior-action)
                                   add-log-messages
                                   (take-warrior-action warrior-action))
            post-env-state (-> post-warrior-state
                               (store-warrior-action nil)
                               remove-dead-units
                               remove-rescued-captives)
            post-npc-state (-> post-env-state
                               take-npc-actions)
            post-env2-state (-> post-npc-state
                                reset-npc-actions
                                check-warrior-dead
                                check-warrior-stalled)]
        (remove nil?
                [post-warrior-state
                 (when (not= post-env-state post-warrior-state)
                   post-env-state)
                 (when (not= post-npc-state post-env-state)
                   post-npc-state)
                 (when (not= post-env2-state post-npc-state)
                   post-env2-state)])))))

(defn play-level [history users-code]
  (if (or
        (:state/game-over? (last history))
        (warrior-at-stairs? (last history)))
    history
    (play-level (concat history
                        (play-turn (last history) users-code)) users-code)))

(defn start-level [level-definition users-code]
  (let [init-state [(extract/generate-initial-level-state level-definition)]]
    (play-level init-state users-code)))

(defn continue-level-state [previous-state level-definition]
  (let [turn (:state/turn previous-state)
        level-state (extract/generate-initial-level-state level-definition)]
    (-> level-state
        (assoc :state/turn turn)
        (assoc :state/messages (vec (concat (:state/messages previous-state)
                                            (map (fn [message]
                                                   (assoc message :message/turn turn))
                                                 (:state/messages level-state))))))))

(defn play-levels [level-definitions users-code]
  (let [history (reduce
                  (fn [memo level-definition]
                    (if (:state/game-over? (last memo))
                      memo
                      (concat memo
                              (play-level
                                [(continue-level-state (last memo) level-definition)]
                                users-code))))
                  [{:state/turn 0
                    :state/messages [{:message/type :message.type/system
                                      :message/text "You enter the tower"
                                      :message/turn 0}]}]
                  level-definitions)]
    (if (:state/game-over? (last history))
      (vec history)
      (update (vec history) (dec (count history))
              add-message "You have reached the top of the tower"))))
