(ns themis.core-test
  (:require [themis.core :refer [validation pvalidation] :as core]
            [themis.predicates :as preds]
            [themis.validators :refer [from-predicate] :as validators]
            [clojure.test :refer :all]))

(def paul {:name {:first "Paul", :last "deGrandis"}
           :has-pet true
           :pets ["walter"]})

(defn degrandis-pets [t-map data-point opt-map]
  (when-not (and (= (get-in t-map [:name :last]) "deGrandis")
                 (:has-pet t-map))
    :no-degrandis-pets-found))

(def paul-rules
  [[[:name :first] [[validators/presence {:response {:text "First name is not there"}}]
                    (fn [t-map data-point opt-map]
                      (and (= data-point "Paul")
                           {:error "You are Paul!"}))]]
   ;; Rules that don't match a specific key validate the whole map
   [:* degrandis-pets]])

(deftest validation-general
  (testing "validation fns return nil or false when validations pass"
    (is (= (validation (assoc-in paul [:name :first] "Bob") paul-rules)
          {[:*] nil, [:name :first] '(nil false)})))
  (testing "validation fns return truish values containing error info when validations fail"
    (is (= (validation (assoc paul :has-pet false) paul-rules)
          {[:*] :no-degrandis-pets-found, [:name :first] '(nil {:error "You are Paul!"})})))
  (testing "rules that validate the whole map can be any name"
    (is (= (get (validation (assoc paul :has-pet false) paul-rules) [:*])
           (get (validation (assoc paul :has-pet false) (assoc paul-rules 1 [:multi_ degrandis-pets])) [:multi_])
          :no-degrandis-pets-found)))
  (testing "pvalidation and validation should validate the same"
    (is (= (validation (assoc paul :has-pet false) paul-rules)
           (pvalidation (assoc paul :has-pet false) paul-rules)))))

(deftest validation-with-different-coordinates
  (testing "a keyword coordinate"
    (is (= (validation {:pets {}} [[:pets [(from-predicate vector? :error)]]])
           {[:pets] :error})))
  (testing "a vector coordinate"
    (is (= (validation {:pets {}} [[[:pets] [(from-predicate vector? :error)]]])
           {[:pets] :error})))
  (testing "a vector coordinate that gets in a vector"
    (is (= (validation {:pets ["Rex"]} [[[:pets 0] [(from-predicate preds/longer-than? 20 :error)]]])
           {[:pets 0] :error})))
  (testing "a vector coordinate that gets in a map"
    (is (= (validation {:pets {:rex "Rex Mister"}} [[[:pets :rex] [(from-predicate preds/longer-than? 20 :error)]]])
           {[:pets :rex] :error}))))

(def name-present (from-predicate seq :not-present))
(def name-long-enough
  (fn [_ data-point opts]
    (when-not (> (count (seq data-point)) 10)
      (or (:error opts) :too-short))))

(deftest validation-with-different-validation-invocations
  (testing "a single fn"
    (is (= (validation {:name nil} [[:name name-present]])
           {[:name] :not-present})))
  (testing "an inline fn"
    (is (= (validation {:name nil} [[:name (from-predicate seq :not-present)]])
           {[:name] :not-present})))
  (testing "a vector with single fn"
    (is (= (validation {:name nil} [[:name [name-present]]])
           {[:name] :not-present})))
  (testing "a vector with multiple fns calls all validations"
    (is (= (validation {:name nil} [[:name [name-present name-long-enough]]])
           {[:name] '(:not-present :too-short)})))
  (testing "a symbol fn"
    (is (= (validation {:name nil} [[:name `name-present]])
           {[:name] :not-present})))
  (testing "a fn with options"
    (is (= (validation {:name nil} [[:name [[name-long-enough {:error :code_105}]]]])
           {[:name] :code_105})))
  (testing "a keyword fn"
    (is (= (validation {:name nil} [[:name ::name-present]])
           {[:name] :not-present}))))

