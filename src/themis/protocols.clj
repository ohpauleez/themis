(ns themis.protocols)

;; Protocols
;; ---------
;;
;; Themis validation is data structure agnostic, but it must be told how
;; it navigates to coordinates within your data structure.

(defprotocol Navigable
  (-navigate [t coordinate-vec]))

