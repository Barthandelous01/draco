(declaim (sb-ext:muffle-conditions cl:style-warning))

(defpackage :notes
  (:export :main :init)
  (:use :cl))
(in-package :notes)

(defun init (arglist)
  (loading:add-to-arglist arglist
			  `("notes" ,#'main "[ep] edit or display notes")))

(defparameter +notes-dir+ (merge-pathnames
			   (make-pathname :directory '(:relative "notes"))
			   globals:+config-folder+)
  "The directory in draco that keeps the notefiles.")

(defparameter +notes-filetype+ "org"
  "The filetype of notes. Change me if you don't like org!")

(defun main (args)
  (cond ((string= (first args) "e") (edit-notes))
	((string= (first args) "p") (print-notes))
	(t (format t "Unknown command: ~a" (first args)))))

(defun edit-notes ()
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

(defun print-notes ()
  (let ((notefile (ensure-directories-exist (merge-pathnames
					     (make-pathname
					      :type +notes-filetype+
					      :name globals:*current-filename*)
					     +notes-dir+))))
    (handler-case (uiop:run-program (list "bat" (namestring notefile))
				    :output :interactive
				    :input :interactive
				    :error-output :interactive)
      (error (e) (format t "Unknown error: ~a~&" e)))))
