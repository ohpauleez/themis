(ns themis.validators)

(def ^:dynamic *default-response* nil)

;; Utility functions
;; -----------------

(defn response
  "Resolve and return a validator's response;
  The value of :response in the opt-map, the response-data passed directly
  to the `response` fn or the default return via *default-response*"
  ([response-data]
   (response response-data {}))
  ([response-data opt-map]
   (or (:response opt-map)
        response-data
        *default-response*)))

(defn from-predicate
  "Given a  predicate function that takes a single arg,
  return a proper validation function for it.
  You can also abuse this for predicates that take multiple arguments;
  The data-point arg is expected to be your first arg (otherwise you should
  just use partial)."
  ([f]
   (simple-predicate f "invalid"))
  ([f response-data]
   (fn [_ data-point _]
     (when-not (f data-point)
       (response response-data {}))))
  ([f arg-data & more-data]
   (fn [_ data-point _]
     (when-not (apply f data-point arg-data (butlast more-data))
       (response (last more-data) {})))))


;; Validation functions
;; ---------------------

(defn required
  "Determine that the coordinate exists in the data structure"
  [t data-point opt-map]
  (let [coords (:themis.core/coordinates opt-map)
        last-coord (last coords)]
    (when-not (contains? (get-in t (butlast coords)) last-coord)
      (response "required key not found" opt-map))))

(defn presence
  "Determine if the data-point is non-nil;
  If there is a value present at a specific coordinate.
  Note: `presence` does not imply `required` - you could fail here because
  the coordinate doesn't actually exist."
  [t data-point opt-map]
  (when (nil? data-point)
    (response "required value is nil" opt-map)))

(comment
  
  )

