(declaim (sb-ext:muffle-conditions cl:style-warning))

(defpackage :strings
  (:use :cl)
  (:export :main :init))
(in-package :strings)

(defun main (args)
  (uiop:run-program (list "strings" globals:*current-file*) :output :interactive))

(defun init (arglist)
  (push `("strings" ,#'main "extract strings from current binary") arglist))
