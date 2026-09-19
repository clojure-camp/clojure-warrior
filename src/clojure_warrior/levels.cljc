(ns clojure-warrior.levels)

(def levels
  [{:level/id 1
    :level/description "You see before yourself a long hallway with stairs at the end. There is nothing in the way."
    :level/tip "Return [:action/walk :direction/forward] from play-turn to walk forward."
    :level/time-bonus 15
    :level/ace-score 10
    :level/board [[:*> nil nil nil nil nil nil :__ nil nil nil]]
    :level/abilities #{:action/walk}}

   {:level/id 2
    :level/description "It is too dark to see anything, but you smell sludge nearby."
    :level/tip "Use (feel board :direction/forward) to see the space in front of you, and return [:action/attack :direction/forward] to fight what is there. Remember, you can only return one action per turn."
    :level/clue "Add an if condition on (:unit/enemy? (feel board :direction/forward)) to decide whether to attack or walk."
    :level/time-bonus 20
    :level/ace-score 26
    :level/board [[:*> nil nil nil :<s nil nil :__ nil nil nil]]
    :level/abilities #{:action/walk :action/attack}}

   {:level/id 3
    :level/description "The air feels thicker than before. There must be a horde of sludge."
    :level/tip "Be careful not to die! Use (:unit/health (warrior board)) to keep an eye on your health, and return [:action/rest] to earn 10% of max health back."
    :level/clue "When there is no enemy ahead of you, rest until your health is full before walking forward."
    :level/time-bonus 35
    :level/ace-score 71
    :level/board [[:*> nil :<s nil :<s :<s nil :<s :__ nil nil]]
    :level/abilities #{:action/walk :action/attack :action/rest}}

   {:level/id 4
    :level/description "You can hear bow strings being stretched."
    :level/tip "No new abilities this time, but you must be careful not to rest while taking damage. Store your health in an atom (use defonce) and compare it each turn to see if you are taking damage."
    :level/clue "Reset the atom to your current health at the end of each turn. If the stored health is greater than your current health, you are taking damage and should not rest."
    :level/time-bonus 45
    :level/ace-score 90
    :level/board [[:*> nil :<S :<a nil :<S :__ nil nil nil nil]]
    :level/abilities #{:action/walk :action/attack :action/rest}}

   {:level/id 5
    :level/description "You hear cries for help. Captives must need rescuing."
    :level/tip "Use (:unit/captive? (feel board :direction/forward)) to see if there is a captive ahead, and return [:action/rescue :direction/forward] to free them. Don't attack captives."
    :level/clue "Don't forget to constantly check if you're taking damage. Rest until your health is full when you are not taking damage."
    :level/time-bonus 45
    :level/ace-score 123
    :level/board [[:*> nil :<C :<a :<a :<S :<C :__ nil nil nil]]
    :level/abilities #{:action/walk :action/attack :action/rest :action/rescue}}

   {:level/id 6
    :level/description "The wall behind you feels a bit further away in this room. And you hear more cries for help."
    :level/tip "You can walk backward with [:action/walk :direction/backward]. The same goes for feel, rescue and attack. Archers have a limited shooting distance."
    :level/clue "Walk backward if you are taking damage from afar and do not have enough health to attack. You may also want to walk backward until (= :unit.type/wall (:unit/type (feel board :direction/backward)))."
    :level/time-bonus 45
    :level/ace-score 90
    :level/board [[:C> nil :*> nil :<S nil :<a :<a :__ nil nil]]
    :level/abilities #{:action/walk :action/attack :action/rest :action/rescue}}

   {:level/id 7
    :level/description "You feel a wall right in front of you and an opening behind you."
    :level/tip "You are not as effective at attacking backward. Check for (= :unit.type/wall (:unit/type (feel board :direction/forward))) and return [:action/pivot] to turn around."
    :level/time-bonus 30
    :level/ace-score 50
    :level/board [[nil nil nil nil nil :__ :a> nil :S> nil :*>]]
    :level/abilities #{:action/walk :action/attack :action/rest :action/rescue :action/pivot}}

   {:level/id 8
    :level/description "You hear the mumbling of wizards. Beware of their deadly wands! Good thing you found a bow."
    :level/tip "Use (look board :direction/forward) to see the first unit ahead of you, and return [:action/shoot :direction/forward] to fire an arrow at anything within 2 spaces."
    :level/clue "Wizards are deadly but low in health. Shoot them before they have time to attack."
    :level/time-bonus 20
    :level/ace-score 46
    :level/board [[:*> nil nil :<C :<w :<w :__ nil nil nil nil]]
    :level/abilities #{:action/walk :action/attack :action/rest :action/rescue :action/pivot :action/shoot}}

   {:level/id 9
    :level/description "Time to hone your skills and apply all of the abilities that you have learned."
    :level/tip "Watch your back."
    :level/clue "Don't just keep shooting the bow while you are being attacked from behind."
    :level/time-bonus 40
    :level/ace-score 100
    :level/board [[:__ :C> :a> nil nil :*> nil :<S nil :<w :<C]]
    :level/abilities #{:action/walk :action/attack :action/rest :action/rescue :action/pivot :action/shoot}}])
