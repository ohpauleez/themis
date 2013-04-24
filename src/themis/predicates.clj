(ns themis.predicates)

;; Predicates
;; -------------
;;

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
     (and (>= length low)
          (<= length high)))))

(defn is-in? [t & items]
  (and (some #{t} items)
       true))

(defn is-not-in? [t & items]
  (not (some #{t} items)))

