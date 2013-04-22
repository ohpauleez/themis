(ns themis.core
  (require [themis.protocols :as protocols]
           [themis.extended-protos :as e-protos]
           [themis.query :as query]))

(defn navigate
  "Fetch the data from within a data structure given coordinates.
  Note: Tuck our internal protocols behind a function for consumption"
  [t coordinate-vec]
  (protocols/-navigate t coordinate-vec))

(defn raw-validation
  "Create a response vector for a validation call,
  given the original data structure, the coordinates,
  the validation function, and the validation optional arg map"
  [t coordinate-vec validation-fn opt-map]
  [coordinate-vec (validation-fn t (navigate t coordinate-vec) opt-map)])

(defn validate-vec
  "Given a single validation query/rule,
  pull apart the constituents and apply a `raw-validation`,
  returning back the validation result vector"
  [t validation-vec]
  (let [[coordinates validations] validation-vec
        [validation-fn opt-map] validations
        opt-map (or opt-map {})]
    (raw-validation t coordinates validation-fn opt-map)))

(defn validation-seq
  "Create a lazy sequence of validating a given data structure
  against the a normalized validation query vector/seq"
  [t normalized-query]
  (map #(validate-vec t %) normalized-query))

(defn validation
  "Validate a data structure, `t`,
  against a normalized validation query/rule-set"
  ([t normalized-query]
   ;; For some reason `doall` doesn't work here
   (apply hash-map (into [] (mapcat identity (validation-seq t normalized-query)))))
  ([t normalized-query merge-fn]
   (merge-fn (validation-seq t normalized-query))))

(comment

  (def paul {:name {:first "Paul", :last "deGrandis"}
             :has-pet true
             :pets ["walter"]})

  (defn w-pets [t-map data-point opt-map]
    (assoc opt-map :pet-name-starts data-point))
  (defn degrandis-pets [t-map data-point opt-map]
    (and (= (get-in t-map [:name :last]) "deGrandis")
         (:has-pet t-map)
         {}))

  (def valid-paul [[[:name :first] [(fn [t-map data-point opt-map] (and (= data-point "Paul")
                                                                        {:a 1 :b 2}))]]
                   [[:pets 0 0] [::w-pets {:pet-name-starts ""}]]
                   ;[[:*] ['degrandis-pets]] ;This is valid, but we can also just write:
                   [:* 'degrandis-pets]])

  (def paul-rules (query/normalize valid-paul))
  (validation-seq paul paul-rules)
  (validation paul paul-rules)
  (validation paul paul-rules identity)

)

