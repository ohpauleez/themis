(ns themis.core
  (require [themis.protocols :as protocols]
           [themis.extended-protos :as e-protos]
           [themis.query :as query]))

;; Themis
;; ---------
;;
;; ### Why another validation library?
;;
;; 1. The validation rule set should be expressed as data
;; 2. Validators should always resolve to something that supports IFn
;; 3. Applying validation should be nothing more than applying functions
;;    and seq'ing the results together
;; 4. Validation is often domain specific, and the library should be open
;;    for modification to adapt easily to different requirements
;;
;; ### Assumptions and expectations
;;
;; Themis attempts to make no assumptions about the data structure you're
;; validation, the results your validator functions return, or how
;; you want to package up the results of a full `validation`.
;;
;; When assumptions are made, there is always an escape hatch allowing
;; you to modify the behavior to better suit your application needs.
;;
;; ### Why the name `Themis`?
;;
;; I was originally using `metis`, a validation library named after Zeus'
;; first wife.  So, I naturally named mine after Zeus' second wife.
;;
;; ### Ideal usage
;; See the comment block below, but `(validation my-ds ds-rule-vec)`

(defn simple-predicate
  "Given a simple predicate function that takes a single arg,
  return a proper validation function for it"
  [f]
  (fn [_ data-point _] (f data-point)))

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
  (let [[coordinates validations] validation-vec]
    (mapcat identity (map (fn [[validation-fn opt-map]]
                            (raw-validation t coordinatesvalidation-fn (merge {::coordinates coordinates} opt-map)))
                          (partition-all 2 validations)))))

(defn validation-seq
  "Create a lazy sequence of validating a given data structure
  against the a normalized validation query vector/seq"
  [t normalized-query]
  (map #(validate-vec t %) normalized-query))

(defn validation
  "Validate a data structure, `t`,
  against a normalized validation query/rule-set
  Note: By default everything is returned in a map, keyed by
  the coordinate vector.  Multiple validation results are conj'd together
  in a vector."
  ([t normalized-query]
   ;; TODO: This can definitely be done better
   (apply merge-with #(conj [%1] %2)
          (flatten (map (fn [result-seq]
                          (map #(apply hash-map %) (partition-all 2 result-seq)))
                        (validation-seq t normalized-query)))))
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
         nil))

  (def valid-paul [[[:name :first] [(fn [t-map data-point opt-map] (and (= data-point "Paul")
                                                                        {:a 1 :b 2}))]]
                   [[:pets 0 0] [::w-pets {:pet-name-starts ""}
                                 (simple-predicate char?)]]
                   ;[[:*] ['degrandis-pets]] ;This is valid, but we can also just write:
                   [:* 'degrandis-pets]])

  (def paul-rules (query/normalize valid-paul))
  (validation-seq paul paul-rules)
  (validation paul paul-rules)
  (mapcat identity (validation paul paul-rules (partial filter second)))

)

