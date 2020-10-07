(defpackage :barthandelous.main
  (:use :cl))
(in-package :barthandelous.main)

;;; util functions for the cli

(defun init-commands ()
  (setf (push `(("help" ,#'help "prints this message")
		("open" ,#'open-project "opens an existing project")
		("new" ,#'new-project "starts a new project")) globals:*command-list*)
	globals:*command-list*))

(defun words (string)
  "An internet-based equivalent to haskell's 'words' function"
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

(defun cli-loop ()
  (loop
    (let* ((line (words (readline)))
	   (command (first line))
	   (args (rest line)))
      (loop for x in globals:*command-list* do
	(if (string= (first x) command)
	    (funcall (second x) args))))))


;;; builtin functions for draco itself. These could be implimented as plugins,
;;; but that would really be a pain.
(defun help ()
  "This prints help for all loaded plugins and builtin commands"
  (loop for y in globals:*command-list* do
    (format t "~a: ~a" (first y) (third y))))

(defun open-project ()
  (format t "working for now"))

(defun new-project ()
  (format t "working for the moment"))

(defun main ()
  (loading:load-all-plugins (loading:init-plugins))
  (cli-loop)
  (sqlite:disconnect globals:*db*))
