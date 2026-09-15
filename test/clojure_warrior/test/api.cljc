(ns clojure-warrior.test.api
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.api :as api]))

(def sample-state
  [[{:unit/type :unit.type/wall}
    {:unit/type :unit.type/warrior
     :unit/direction :direction/east}
    {:unit/type :unit.type/floor}
    {:unit/type :slug}
    {:unit/type :unit.type/archer}
    {:unit/type :unit.type/stairs}
    {:unit/type :unit.type/wall}]])

(deftest stairs
  (testing "stairs"
    (is (= {:unit/type :unit.type/stairs
            :unit/position [5 0]}
           (api/stairs sample-state)))))

(deftest warrior
  (testing "warrior"
    (is (= {:unit/type :unit.type/warrior
            :unit/direction :direction/east
            :unit/position [1 0]}
           (api/warrior sample-state)))))

(deftest look
  (testing "look"
    (is (= {:unit/type :slug
            :unit/position [3 0]}
           (api/look sample-state :direction/forward)))))

(deftest feel
  (testing "feel"
    (is (= {:unit/type :unit.type/floor
            :unit/position [2 0]}
           (api/feel sample-state :direction/forward)))))

(deftest listen
  (testing "listen"
    (is (= [{:unit/type :slug
             :unit/position [3 0]}
            {:unit/type :unit.type/archer
             :unit/position [4 0]}]
           (api/listen sample-state)))))

(deftest distance-to
  (testing "distance-to"
    (is (= 1 (api/distance-to sample-state [0 0])))
    (is (= 6 (api/distance-to sample-state [4 3])))))

(deftest inspect
  (testing "inspect"
    (is (= {:unit/type :unit.type/archer
            :unit/position [4 0]}
           (api/inspect sample-state [4 0])))))

(deftest say
  (testing "say sends joined text to *say-listener*"
    (let [collected (atom [])]
      (binding [api/*say-listener* (fn [text]
                                     (swap! collected conj text))]
        (api/say "health:" 10.0)
        (api/say {:a 1}))
      (is (= ["health: 10.0" "{:a 1}"]
             @collected)))))
