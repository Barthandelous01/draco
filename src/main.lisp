(defpackage :barthandelous.main
  (:use :cl)
  (:nicknames :main)
  (:export :main :init-commands :cli-loop :help))
(in-package :barthandelous.main)

;;; util functions for the cli

(defun init-commands ()
  (setf globals:*command-list*
	(push `("help" ,#'help "prints this message") globals:*command-list*))
  (setf globals:*command-list*
	(push `("open" ,#'open-project "opens a new project") globals:*command-list*))
  (setf globals:*command-list*
	(push `("new" ,#'new-project "starts a new project") globals:*command-list*))
  (setf globals:*command-list*
	(push `("quit" ,#'uiop:quit "quits draco") globals:*command-list*)))

(defun words (string)
  "An internet-based equivalent to haskell's 'words' function"
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

(defun cli-loop ()
					;  (loop
  (let* ((line (words (read-line)))
	 (command (first line))
	 (args (rest line)))
    (loop for x in globals:*command-list* do
	  (if (string= (first x) command)
	      (funcall (second x) args)))));)


;;; builtin functions for draco itself. These could be implimented as plugins,
;;; but that would really be a pain.
(defun help (_)
  "This prints help for all loaded plugins and builtin commands"
  (loop for y in globals:*command-list* do
	(format t "~a: ~a~&" (first y) (third y))))

(defun open-project (args)
  "This basically loads the 'current file' variable from the sqlite db"
  (setf globals:*current-file*
	(sqlite:execute-single globals:*db* "select FILEPATH from files where name = ?"
			       (first args))))

(defun new-project (_)
  (format t "working for the moment"))

(defun main ()
  "The main function. Runs the main loop, plus some init and shutdown."
  (unwind-protect
      (progn
	(init-commands)
	(setf globals:*db* (sqlite:connect globals:+db-file+))
	(sqlite:execute-non-query globals:*db*
				  "create table if not exists files (
             ID        INTEGER PRIMARY KEY AUTOINCREMENT,
             NAME      TEXT NOT NULL,
             FILEPATH  TEXT NOT NULL);")
	(loading:load-all-plugins (loading:init-plugins))
	(cli-loop))
    (progn
      (sqlite:disconnect globals:*db*)
      (format t "something threw an error"))))
