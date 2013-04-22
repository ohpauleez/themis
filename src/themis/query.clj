(ns themis.query)

;; Themis Validation queries
;; --------------------------
;;
;; ### Structure
;; The query is specifically just data that gets ingested into
;; some validation engine.  This has a few key benefits like
;; composability and packaging/serializing.
;;
;; A query is a vector of vector pairs.
;; In its short form:
;; `[[:coordinate validation-fn]]`
;;
;; In its long form:
;; `[[[:coordinate] [vaildation-fn]]`
;;
;; The query vectory pair is some coordinate into the data structure you're
;; validation, and the validation function that should be applied at that
;; location.
;; Care has been taken to make it work well with hash maps, but it should
;; work equally well with other data structures - the engine is open for
;; modification.

(defn balanced?
  "Ensure that every key/map selection
  is paired to some validation symbol/keyword/vec"
  [validation-vec]
  (try
    (every? #(even? (count %)) validation-vec)
    (catch UnsupportedOperationException uoe
      (throw (Exception. (str "The validation vector must only contain vectors; " (.getMessage uoe))
                         uoe)))))

(defn nsed-name
  "Like `name`, but respects namespaces"
  [sym-or-kw]
  (let [tname (name sym-or-kw)
        tns (namespace sym-or-kw)]
    (if tns
      (str tns "/" tname)
      tname)))

;; TODO potentially protocol this
(defn- normalize-item
  "Given an element in a validation query, resolve it to a function"
  ([validation-item]
   (normalize-item validation-item #(throw (Exception. (str "Validation items must be symbols, keywords, strings, or functions. Not: " (type %))))))
  ([validation-item else-fn]
  (cond
    (symbol? validation-item) @(resolve validation-item)
    (keyword? validation-item) @(resolve (symbol (nsed-name validation-item)))
    (string? validation-item) @(resolve (symbol validation-item))
    (instance? clojure.lang.IFn validation-item) validation-item
    :else (else-fn validation-item))))

(defn normalize-validation-fns
  "Given the validation function vectors,
  normalize them; resolving symbols/keywords to actual functions"
  [validation-fn-vec]
  (mapv #(normalize-item % identity) validation-fn-vec))

(defn- vectorize
  "Properly wrap query items in vectors"
  ([x]
   (vectorize x vector))
  ([x vector-fn]
   (if (vector? x) x (vector-fn x))))

(defn normalize
  "Ensure all coordinates and validators are in a vector
  (or vector of vectors);
  Ensure all validation functions are fully resolved"
  [validation-vec]
  (map (fn [[coordinates validation]]
         [(vectorize coordinates) (-> validation vectorize normalize-validation-fns)])
       validation-vec))


(comment
 
  (def example-map {:foo {:bar [5 6 7]}, :alpha 1})
  (defn valid? [whole-map kw-vec opt-map] {})
  (def valid-query [[[[:foo] [:foo :bar]] "valid?"]
                    [:alpha [valid? {:another-opt true}]]])
  (def short-query [[:foo valid?]])

  (balanced? (normalize valid-query))
  (balanced? short-query)
  (balanced? [[:a :b] :c])
  (normalize short-query)
 
 ;; Don't enforce what the keys actually have to be;
  ;; leave this here for reference for now 
  (def ^:dynamic *coordinate-type-set* #{clojure.lang.Keyword clojure.lang.PersistentVector})
  (every? #(coordinate-type-set (type %)) (take-nth 2 validations-vec))
  (interleave [0 2 4] [1 3 5])
  (symbol 'normalize-query-structure)
  (symbol (nsed-name ::something))
  (nsed-name "themis.core/normalized-query-structure")

  (symbol count)

  
)
 
