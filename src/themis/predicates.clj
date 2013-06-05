(ns themis.predicates)

;; Predicates
;; -------------
;;
;; It is often easier to reason about validtion composing smaller
;; predicate functions.  Below you'll find common ones supplied by Themis.
;;
;; These also serve as an example of how two write application specific
;; validators

(defn longer-than? [t length]
  (> (count t) length))

(defn shorter-than? [t length]
  (< (count t) length))

(defn length? [t length]
  (= (count t) length))

(defn length-between?
  ([t high]
   (length-between? t 0 high))
  ([t low high]
   (let [length (count t)]
     (<= low length high))))

(defn is-in? [t & items]
  (and (some #{t} items)
       true))

(defn is-not-in? [t & items]
  (not (some #{t} items)))

