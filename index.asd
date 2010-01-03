;;; index index.asd - Andrew Stine (c) 2009

(asdf:defsystem :index
  :description "index"
  :version "0.0.1"
  :serial t
  :components ((:file "index"))
  :depends-on (:cl-fad
	       :unix-options
	       :trivial-shell
	       :rucksack))
