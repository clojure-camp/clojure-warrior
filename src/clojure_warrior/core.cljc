(ns clojure-warrior.core
  #?(:clj (:gen-class))
  (:require
    [clojure-warrior.api :as api]
    [clojure-warrior.play :as play]))

(def feel api/feel)
(def look api/look)
(def listen api/listen)
(def inspect api/inspect)

(def stairs api/stairs)
(def warrior api/warrior)

(def say api/say)

(def distance-to api/distance-to)


(defn enter-the-tower!
  [user-code]
  (let [levels [{:level/id 1
                 :level/board [[:*> :-- :<a :__]]}
                {:level/id 2
                 :level/board [[:*> :__]]}]]
    (play/play-levels levels user-code)))


(def play-levels! play/play-levels)
