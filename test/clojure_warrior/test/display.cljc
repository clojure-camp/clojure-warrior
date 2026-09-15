(ns clojure-warrior.test.display
  (:require
    [clojure.test :refer [deftest testing is]]
    [clojure-warrior.display :as display]))

(deftest generate-display
  (testing "generate-display"
    (let [in {:state/board
              [[{:unit/type :unit.type/wall
                 :unit/display-char \|}
                {:unit/type :unit.type/captive
                 :unit/display-char \C}
                {:unit/type :unit.type/archer
                 :unit/display-char \a}
                {:unit/type :unit.type/floor
                 :unit/display-char " "}
                {:unit/type :unit.type/wall
                 :unit/display-char \|}]]}
          out "-----\n|Ca |\n-----"]
      (is (= out (display/generate-display in))))))
