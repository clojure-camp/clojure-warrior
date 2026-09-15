(ns clojure-warrior.test.state
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.state :as state]))

(deftest get-warrior
  (testing "get-warrior"
    (is (= {:unit/type :unit.type/warrior
            :unit/position [2 0]}
           (state/get-warrior [[{} {} {:unit/type :unit.type/warrior}]])))))

(deftest get-stairs
  (testing "get-stairs"
    (is (= {:unit/type :unit.type/stairs
            :unit/position [2 0]}
           (state/get-stairs [[{} {} {:unit/type :unit.type/stairs}]])))))

(deftest unit-at-position
  (testing "unit-at-position"
    (is (= {:unit/type :unit.type/warrior
            :unit/position [0 0]}
           (state/unit-at-position [[{:unit/type :unit.type/warrior}]] [0 0])))))

(deftest add-message
  (testing "wraps strings as system messages"
    (is (= {:state/messages [{:message/type :message.type/system
                              :message/text "hello"}]}
           (state/add-message {:state/messages []} "hello"))))

  (testing "keeps message maps as-is"
    (is (= {:state/messages [{:message/type :message.type/say
                              :message/text "hi"}]}
           (state/add-message {:state/messages []} {:message/type :message.type/say
                                                    :message/text "hi"})))))
