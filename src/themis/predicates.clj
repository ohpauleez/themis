(ns themis.predicates)

;; Predicates
;; -------------
;;
;; It is often easier to reason about validation composing smaller
;; predicate functions.  Below you'll find common ones supplied by Themis.
;;
;; These also serve as an example of how two write application specific
;; validators

(defn longer-than?
  "Returns true if an item, `t`, is longer than some `length`, non-inclusive"
  [t length]
  (> (count t) length))

(defn shorter-than?
  "Returns true if an item, `t`, is shorter than some `length`, non-inclusive"
  [t length]
  (< (count t) length))

(defn length?
  "Returns true if an item, `t`, is as long as `length`"
  [t length]
  (= (count t) length))

(defn length-between?
  "Returns true if an item, `t`, is within some length, inclusive.
  If you only supply a high-bound, the low defaults to 0"
  ([t high]
   (length-between? t 0 high))
  ([t low high]
   (let [length (count t)]
     (<= low length high))))

(defn is-in?
  "Returns true if an item, `t`, is matched/present in a collection of `items`"
  [t & items]
  (and (some #{t} items)
       true))

(defn is-not-in?
  "Returns true if an item, `t`, is not matched/present in a collection of `items`"
  [t & items]
  (not (some #{t} items)))

(def non-neg-integer?
  "Returns true if an integer and >= 0"
  (every-pred integer? (complement neg?)))

(defn boolean? [t]
  (instance? java.lang.Boolean t))

(defn format?
  "Returns true if a string, `s`, is of a specific format; if it matches a regex string.
  If the regex string is passed in as a string, it will be converted to a RegEx object"
  [s regex-str]
  (let [regex-str (if (string? regex-str) (re-pattern regex-str) regex-str)]
    (boolean (re-find regex-str s))))

