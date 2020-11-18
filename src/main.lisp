(defpackage :barthandelous.main
  (:use :cl)
  (:nicknames :main)
  (:export :main :init-commands :cli-loop :help :new-project))
(in-package :barthandelous.main)

;;; util functions for the cli

(defun words (string)
  "An internet-based equivalent to haskell's 'words' function"
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
	while j))

(defun bulk-copy (infile outfile)
  "Directly copies a file with chunks.
   https://riptutorial.com/common-lisp/example/18886/copying-a-file"
  (with-open-file (instream (ensure-directories-exist infile) :direction :input :element-type '(unsigned-byte 8)
							      :if-does-not-exist nil)
    (when instream
      (with-open-file (outstream (ensure-directories-exist outfile) :direction :output :element-type '(unsigned-byte 8)
								    :if-exists :supersede)
	(let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
	  (loop for bytes-read = (read-sequence buffer instream)
		while (plusp bytes-read)
		do (write-sequence buffer outstream :end bytes-read)))))))


;;; cli-specific commands

(defun init-commands ()
  "This function is basically the init for the 'core' plugin and functionality."
  (loading:add-to-arglist globals:*command-list*
			  `("help" ,#'help "prints this message")
			  `("new" ,#'new-project "[name] [path]: starts a new project")
			  `("open" ,#'open-project "[name]: opens a new project")
			  `("quit" ,#'(lambda (_) (error "quiting")) "quits draco")))

(define-condition unknown-command-error (error)
  ((text :initarg :text :reader :text)))

(defun cli-parse ()
  "The cli parser function. Much more elegant than todo-list's."
  (format t "~&~a"
	  (concatenate 'string globals:*current-filename* globals:+prompt+))
  (finish-output)
  (let* ((line (words (read-line)))
	 (command (first line))
	 (args (rest line)))
    (loop for x in globals:*command-list* do
	  (if (string= (first x) command)
	      (progn
		(funcall (second x) args)
		(return-from cli-parse t))))
    (error 'unknown-command-error :text command)))

(defun cli-loop ()
  "A loop around the cli-parser function which allows for better error handling"
  (loop
   (handler-case (cli-parse)
     (unknown-command-error (err) (format t "Unknown command ~a~%" (slot-value err 'text)))
     (error (e) (error e)))))


;;; builtin functions for draco itself. These could be implimented as plugins,
;;; but that would really be a pain.

(defun help (_)
  "This prints help for all loaded plugins and builtin commands"
  (loop for y in globals:*command-list* do
	(format t "~15a: ~a~&" (first y) (third y))))

(defun open-project (args)
  "This basically loads the 'current file' variable from the sqlite db"
  (let ((file
	 (sqlite:execute-single globals:*db*
				"select FILEPATH from files where name = ?" (first args))))
    (if  (null file)
	(format t "~&Unable to open project '~a'" (first args))
      (progn
	(setf globals:*current-file* file)
	(setf globals:*current-filename* (first args))))))


(defun new-project (args)
  "this starts a new project"
  (let* ((name (first args))
	 (file (second args))
	 (dest (merge-pathnames name
				globals:+binary-folder+)))
    (progn
      (format t "Copying ~a to ~a~%" file dest)
      (bulk-copy file dest)
      (sqlite:execute-non-query globals:*db* "insert into files (NAME, FILEPATH) values (?, ?)" name (namestring dest))))
  (finish-output))

;;; main; toplevel entry point

(defun main ()
  "The main function. Runs the main loop, plus some init and shutdown."
  (unwind-protect
      (progn
	(init-commands)
	(setf globals:*db* (sqlite:connect (ensure-directories-exist globals:+db-file+)))
	(sqlite:execute-non-query globals:*db* "create table if not exists files (
				   ID        INTEGER PRIMARY KEY AUTOINCREMENT,
				   NAME      TEXT NOT NULL,
			           FILEPATH  TEXT NOT NULL);")
	(loading:load-all-plugins (loading:init-plugins))
	(handler-case (cli-loop)
	  (error (e) (terpri))))
    (progn
      (sqlite:disconnect globals:*db*)
      (uiop:quit))))
