(declaim (sb-ext:muffle-conditions cl:style-warning))

(defpackage :R2
  (:use :cl)
  (:export :init :main))
(in-package :R2)

(defun init (arglist)
  (push `("r2" ,#'main "Run radare2 on the file") arglist))

(defun main (_)
  (uiop:run-program (list "r2" globals:*current-file*) :output :interactive :input :interactive :error-output :interactive :wait t))
