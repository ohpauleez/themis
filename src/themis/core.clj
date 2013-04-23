(ns themis.core
  (require [themis.protocols :as protocols]
           [themis.extended-protos :as e-protos]
           [themis.rules :as rules]))

;; Themis
;; ---------
;;
;; ### Why another validation library?
;;
;; 1. The validation rule set should be expressed as data.
;;    Rule sets should be able to completely serialize to EDN.
;; 2. Validators should always resolve to something that supports IFn.
;;    The resolution of data to IFn-ables happens at validation time.
;; 3. Applying validation should be nothing more than applying functions
;;    and seq'ing the results together.
;; 4. Validation is often domain specific, and the library should be open
;;    for modification to adapt easily to different requirements.
;; 5. Validation results should not be conj'd/merge'd onto the original data
;;    (unless the user specifically did that).
;; 6. Validation rule sets should allow for proper namespacing of
;;    validators (symbols/functions/etc) and general data.
;;
;; ### Assumptions and expectations
;;
;; Themis attempts to make no assumptions about the data structure you're
;; validating, the results your validator functions return, or how
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
;;
;; See the comment block below, but `(validation my-ds ds-rule-vec)`

(defn navigate
  "Fetch the data from within a data structure given coordinates.
  If the data structure is not extended to Navigable, try `get-in`.
  Note: Tuck our internal protocols behind a function for consumption"
  [t coordinate-vec]
  (if (satisfies? protocols/Navigable t)
    (protocols/-navigate t coordinate-vec)
    (get-in t coordinate-vec)))

(defn raw-validation
  "Create a response vector for a validation call,
  given the original data structure, the coordinates,
  the validation function, and the validation optional arg map"
  [t coordinate-vec validation-fn opt-map]
  [coordinate-vec (validation-fn t (navigate t coordinate-vec) opt-map)])

(defn validate-vec
  "Given a single validation rule,
  pull apart the constituents and apply a `raw-validation`,
  returning back the validation result vector"
  [t validation-vec]
  (let [[coordinates validations] validation-vec]
    (partition-all 2
      (mapcat (fn [[validation-fn opt-map]]
                (raw-validation t coordinates validation-fn (merge {::coordinates coordinates} opt-map)))
              validations))))

(defn validation-seq
  "Create a lazy sequence of validating a given data structure
  against the a normalized validation rule-set vector/seq"
  [t normalized-query]
  (when (rules/balanced? normalized-query)
    (mapcat #(validate-vec t %) normalized-query)))

(defn validation
  "Validate a data structure, `t`,
  against validation query/rule-set.  The rule-set will be normalized if
  it is not already.
  Note: By default everything is returned in a map, keyed by
  the coordinate vector.  Multiple validation results are conj'd together
  in a vector."
  ([t rule-set]
   ;; TODO: This can definitely be done better
   (apply merge-with #(flatten (concat [%1] [%2]))
          (mapcat (fn [result-seq]
                    (map #(apply hash-map %) (partition-all 2 result-seq)))
                  (validation-seq t (rules/normalize rule-set)))))
  ([t rule-set merge-fn]
   (merge-fn (validation-seq t (rules/normalize rule-set)))))

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

  (require '[themis.validators :refer [simple-predicate]])

  (def paul-rules [[[:name :first] [(fn [t-map data-point opt-map] (and (= data-point "Paul")
                                                                        {:a 1 :b 2}))]]
                   [[:pets 0 0] [[::w-pets {:pet-name-starts ""}]
                                 (simple-predicate char?)
                                 (simple-predicate #(= % \w) "The first letter is not `w`")]]
                   ;[[:*] ['degrandis-pets]] ;This is valid, but we can also just write:
                   [:* 'degrandis-pets]])

  (def normal-paul-rules (rules/normalize paul-rules))
  (type (validation-seq paul normal-paul-rules))
  (validation paul paul-rules)
  (mapcat identity (validation paul paul-rules (partial filter second)))
  (validation paul paul-rules (partial keep second))

)

