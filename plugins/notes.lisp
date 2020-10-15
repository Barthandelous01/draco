(declaim (sb-ext:muffle-conditions cl:style-warning))

(defpackage :notes
  (:export :main :init)
  (:use :cl))
(in-package :notes)

(defun init (arglist)
  (loading:add-to-arglist arglist
			  `("notes" ,#'main "open the notes file for the project")))

  (defparameter +notes-dir+ (merge-pathnames
			     (make-pathname :directory '(:relative "notes"))
			     globals:+config-folder+)
    "The directory in draco that keeps the notefiles.")

(defparameter +notes-filetype+ "org"
  "The filetype of notes. Change me if you don't like org!")

(defun main (_)
  (let ((notefile (ensure-directories-exist (merge-pathnames
					     (make-pathname
					      :type +notes-filetype+
					      :name globals:*current-filename*)
					     +notes-dir+))))
    (handler-case (uiop:run-program (list (uiop:getenv "VISUAL")
					  (namestring notefile))
				    :output :interactive
				    :input :interactive
				    :error-output :interactive)
      (error (e) (format t "error opening editor~&")))))
