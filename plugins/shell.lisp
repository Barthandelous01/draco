(declaim (sb-ext:muffle-conditions cl:style-warning))

(defpackage :shell
  (:use :cl)
  (:export :main :init))
(in-package :shell)

(defun main (_)
  (uiop:run-program "sh" :output :interactive :input :interactive :wait t))

(defun init (arglist)
  (push `("shell" ,#'main "spawn a subshell") arglist))
