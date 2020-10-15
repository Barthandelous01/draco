(declaim (sb-ext:muffle-conditions cl:style-warning))

(defpackage :ls
  (:use :cl)
  (:export :main :init))
(in-package :ls)

(defun init (arglist)
  (loading:add-to-arglist arglist
			  `("ls" ,#'main "list available projects")))

(defun main (_)
  (format t "~{~{~a~&~}~}"
	  (sqlite:execute-to-list
	   globals:*db*
	   "select NAME from files;")))
