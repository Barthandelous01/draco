(declaim (sb-ext:muffle-conditions cl:style-warning))

(defpackage :elf  
  (:use :cl)
  (:export :init :main))
(in-package :elf)

(defun init (arglist)
  (push `("elf" ,#'main "extract elf headers") arglist))

(defun main (args)  
  (uiop:run-program (list "readelf" "-a" globals:*current-file*) :input :interactive :output :interactive :wait t))

