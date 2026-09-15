(ns clojure-warrior.test.extract
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.extract :as extract]))

(deftest extract-unit
  (testing "extract unit type correctly"
    (is (= :unit.type/wall (:unit/type (extract/extract-unit :--))))
    (is (= :unit.type/captive (:unit/type (extract/extract-unit :C>))))
    (is (= :unit.type/archer (:unit/type (extract/extract-unit :a>))))
    (is (= :unit.type/warrior (:unit/type (extract/extract-unit :*>)))))

  (testing "extract unit direction correctly"
    (is (= :direction/east (:unit/direction (extract/extract-unit :C>))))
    (is (= :direction/west (:unit/direction (extract/extract-unit :<C))))))

(deftest extract-board
  (testing "extract-board"
    (let [in [[:__ :C> :a> nil nil :*> nil :<S nil :<w :<C]]
          expected [[{:unit/type :unit.type/wall}
                     {:unit/type :unit.type/stairs}
                     {:unit/type :unit.type/captive
                      :unit/direction :direction/east
                      :unit/health 1.0}
                     {:unit/type :unit.type/archer
                      :unit/direction :direction/east
                      :unit/health 7.0}
                     {:unit/type :unit.type/floor}
                     {:unit/type :unit.type/floor}
                     {:unit/type :unit.type/warrior
                      :unit/health 20.0
                      :unit/direction :direction/east}
                     {:unit/type :unit.type/floor}
                     {:unit/type :unit.type/thick-sludge
                      :unit/direction :direction/west
                      :unit/health 24.0}
                     {:unit/type :unit.type/floor}
                     {:unit/type :unit.type/wizard
                      :unit/direction :direction/west
                      :unit/health 3.0}
                     {:unit/type :unit.type/captive
                      :unit/direction :direction/west
                      :unit/health 1.0}
                     {:unit/type :unit.type/wall}]]
          out (extract/extract-board in)]
      (is (= (count (first expected))
             (count (first out))))
      (is (= (map :unit/type (first expected))
             (map :unit/type (first out))))
      (is (= (map :unit/direction (first expected))
             (map :unit/direction (first out))))
      (is (= (map :unit/health (first expected))
             (map :unit/health (first out)))))))
