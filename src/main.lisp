(defpackage :barthandelous.main
  (:use :cl)
  (:nicknames :main)
  (:export :main :init-commands :cli-loop :help))
(in-package :barthandelous.main)

;;; util functions for the cli

(defun bulk-copy (infile outfile)
  "Directly copies a file with chunks.
   https://riptutorial.com/common-lisp/example/18886/copying-a-file"
  (with-open-file (instream infile :direction :input :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
		  (when instream
		    (with-open-file (outstream outfile :direction :output :element-type '(unsigned-byte 8)
					       :if-exists :supersede)
				    (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
				      (loop for bytes-read = (read-sequence buffer instream)
					    while (plusp bytes-read)
					    do (write-sequence buffer outstream :end bytes-read)))))))

(defun init-commands ()
  "This is a hackish way to set this up, but it's the only reasonable way."
  (setf globals:*command-list*
	(push `("help" ,#'help "prints this message") globals:*command-list*))
  (setf globals:*command-list*
	(push `("open" ,#'open-project "[name]: opens a new project") globals:*command-list*))
  (setf globals:*command-list*
	(push `("new" ,#'new-project "[name] [path]: starts a new project") globals:*command-list*))
  (setf globals:*command-list*
	(push `("quit" ,#'(lambda (_) (error "quiting")) "quits draco") globals:*command-list*)))

(defun words (string)
  "An internet-based equivalent to haskell's 'words' function"
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

(defun cli-loop ()
  "the loop that runs the CLI"
  (handler-case
      (loop
       (format t "~&~a"
	       (concatenate 'string globals:*current-filename* globals:+prompt+))
       (finish-output)
       (let* ((line (words (read-line)))
	      (command (first line))
	      (args (rest line)))
	 (loop for x in globals:*command-list* do
	       (if (string= (first x) command)
		   (funcall (second x) args))))))
  (error () (error "exiting")))


;;; builtin functions for draco itself. These could be implimented as plugins,
;;; but that would really be a pain.
(defun help (_)
  "This prints help for all loaded plugins and builtin commands"
  (loop for y in globals:*command-list* do
    (format t "~a: ~a~&" (first y) (third y))))

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
	 (dest (concatenate 'string globals:+binary-folder+ "/" name)))
    (progn
      (format t "Copying ~a to ~a~%" file dest)
      (bulk-copy file dest)
      (sqlite:execute-non-query globals:*db* "insert into files (NAME, FILEPATH) values (?, ?)" name dest)))
  (finish-output))

(defun main ()
  "The main function. Runs the main loop, plus some init and shutdown."
  (unwind-protect
      (progn
	(init-commands)
	(setf globals:*db* (sqlite:connect globals:+db-file+))
	(sqlite:execute-non-query globals:*db* "create table if not exists files (
				   ID        INTEGER PRIMARY KEY AUTOINCREMENT,
				   NAME      TEXT NOT NULL,
			           FILEPATH  TEXT NOT NULL);")
	(loading:load-all-plugins (loading:init-plugins))
	(handler-case (cli-loop)
	  (error (e) (format t "~&"))))
    (progn
      (sqlite:disconnect globals:*db*)
      (uiop:quit))))
