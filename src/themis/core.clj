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
  [(get opt-map ::return-coordinates coordinate-vec) (validation-fn t (navigate t coordinate-vec) opt-map)])

(defn validate-vec
  "Given a single validation rule,
  pull apart the constituents and apply a `raw-validation`,
  returning back the validation result vector"
  [t validation-vec]
  (let [[coordinates validations] validation-vec]
    (partition-all 2
      (mapcat (fn [[validation-fn opt-map]]
                (raw-validation t coordinates validation-fn (assoc opt-map ::coordinates coordinates)))
              validations))))

(defn validation-seq
  "Create a lazy sequence of validating a given data structure
  against a validation rule-set vector/seq.
  The resulting seq is of `(coordinate validation-result)` tuples/seq"
  [t rule-set]
  (let [normalized-rules (rules/normalize rule-set)]
    (when (rules/balanced? normalized-rules)
      (mapcat #(validate-vec t %) normalized-rules))))

(defn validation-seq->map
  ""
  [validation-seq]
   ;; TODO: This can definitely be done better
  (apply merge-with #(flatten (concat [%1] [%2]))
          (mapcat (fn [result-seq]
                    (map #(apply hash-map %) (partition-all 2 result-seq)))
                  validation-seq)))

(defn validation
  "Validate a data structure, `t`,
  against validation query/rule-set.
  The rule-set will be normalized if it is not already.
  Note: By default everything is returned in a map, keyed by
  the coordinate vector.
  Multiple validation results are conj'd together in a vector.
  You can optionally pass in a custom :merge-fn or :validation-seq-fn to process
  the validation and tailor the results"
  [t rule-set & opts]
  (let [{:keys [merge-fn validation-seq-fn]} (merge {:merge-fn validation-seq->map
                                                     :validation-seq-fn validation-seq}
                                                    (apply hash-map opts))]
    (merge-fn (validation-seq-fn t rule-set))))

(defn pvalidation-seq
  "Like `validation-seq`, but chunks rules based on the number of
  available cores, and validates the chunks in parallel."
  [t rule-set]
  (let [normalized-rules (rules/normalize rule-set)
        chunks (.availableProcessors (Runtime/getRuntime))
        rule-count (count normalized-rules)
        chunked-rules (vec (partition-all (/ rule-count chunks) normalized-rules))
        validate-vec-fn #(validate-vec t %)]
    (when (rules/balanced? normalized-rules)
      (into [] (mapcat deref
                       (map #(future (mapcat validate-vec-fn %))
                            chunked-rules))))))

(defn pvalidation
  "Like `validation`, but will create the validation-seqs in parallel via
  `pvalidation-seq` - which is based on the number of recognized cores.
  Note: :validation-seq-fn is ignored in this call."
  [t rule-set & {:keys [merge-fn]}]
  (validation t rule-set
              :validation-seq-fn pvalidation-seq
              :merge-fn (or merge-fn validation-seq->map)))

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

  (require '[themis.validators :refer [from-predicate presence]])
  (require '[themis.predicates :as preds])

  (def paul-rules [[[:name :first] [[presence {:response {:text "First name is not there"}}]
                                    (fn [t-map data-point opt-map](Thread/sleep 500)(and (= data-point "Paul")
                                                                        {:a 1 :b 2}))]]
                   [[:pets 0] [(from-predicate preds/longer-than? 20 "Too short; Needs to be longer than 20")]]
                   [[:pets 0 0] [[::w-pets {:pet-name-starts ""}]
                                 (from-predicate char?)
                                 (from-predicate #(or (Thread/sleep 200) (= % \w)) "The first letter is not `w`")]]
                   ;[[:*] ['degrandis-pets]] ;This is valid, but we can also just write:
                   [:* 'degrandis-pets]])

  (def normal-paul-rules (rules/normalize paul-rules))
  (type (validation-seq paul paul-rules))
  (type (pvalidation-seq paul paul-rules))
  (time (validation paul paul-rules))
  (time (pvalidation paul paul-rules))
  (= (validation paul paul-rules) (pvalidation paul paul-rules))
  (mapcat identity (validation paul paul-rules :merge-fn (partial filter second)))
  (validation paul paul-rules :merge-fn (partial keep second))
  

  (defn unfold-result
    "Unfold the themis results map, expanding coordinates to nested maps,
    and remove `nil` results"
    [themis-result-map]
    (reduce (fn [old [k-vec value]]
              (let [validation-value (remove nil? value)
                    seqd-value (not-empty validation-value)]
                (if seqd-value
                  (assoc-in old k-vec
                            (if (sequential? value)
                              (vec seqd-value)
                              value))
                  old)))
            nil themis-result-map))
  (unfold-result (validation paul paul-rules))

)

