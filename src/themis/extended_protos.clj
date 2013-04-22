(ns themis.extended-protos
  (:require [themis.protocols :as protocols]))

(extend-protocol protocols/Navigable

  clojure.lang.PersistentVector
  (-navigate [t coordinate-vec]
    (get-in t coordinate-vec))

 clojure.lang.IPersistentMap
 (-navigate [t coordinate-vec]
   (get-in t coordinate-vec))
  
  java.lang.String
  (-navigate [t coordinate-vec]
    (get-in t coordinate-vec)))

