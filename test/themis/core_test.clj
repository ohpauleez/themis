(ns themis.core-test
  (:require [themis.core :as core]
            [themis.predicates :as preds]
            [themis.validators :refer [from-predicate presence]]
            [clojure.test :refer :all]))

(deftest simple-tests
  (let [alice {:name "Alice"}
        alice-rules [[[:name]
                      (fn [_payload value opts]
                        (when (not= value "Alice")
                          {:error ":name not Alice"}))]]]

    (testing "Alice's name should be Alice."
      (is (= (core/validation alice alice-rules) {[:name] nil})))
    (testing "Bob's name should not be Alice."
      (is (not= (core/validation {:name "Bob"} alice-rules) {[:name] nil})))))

(deftest pauls-tests
  "These are selected tests are ported from Paul DeGrandis' comment tests."
  (let [paul {:name {:first "Paul", :last "deGrandis"}
              :has-pet true
              :pets ["walter"]}
        paul-rules [[[:name :first] [[presence {:response {:text "First name is not there"}}]
                                     (fn [t-map data-point opt-map](Thread/sleep 500)(and (= data-point "Paul")
                                                                                          {:a 1 :b 2}))]]
                    [[:pets 0] [(from-predicate preds/longer-than? 20 "Too short; Needs to be longer than 20")]]]
        paul-validation {[:name :first]  '(nil  {:a 1, :b 2}), [:pets 0] "Too short; Needs to be longer than 20"}]
    (testing "Does validation work on paul-rules?"
      (is (= (core/validation paul paul-rules)
             paul-validation)))
    (testing "Does pvalidation work on paul-rules?"
      (is (= (core/pvalidation paul paul-rules)
             paul-validation)))))
