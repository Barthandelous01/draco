(defpackage #:draco-asd
  (:use :cl :asdf))
(in-package #:draco-asd)

(defsystem draco
  :name "draco"
  :version "0.0.1"
  :maintainer "Barthandelous01"
  :author "Barthandelous01"
  :license "3-Clause BSD"
  :depends-on (:dexador :sqlite)
  :description "A malware analysis framework"
  :long-description "A minimal, plugin based malware analysis framework"
  :serial t
  :components ((:file "globals")
	       (:file "load")
	       (:file "main")))
