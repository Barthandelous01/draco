(defpackage :barthandelous.load
  (:use :cl)
  (:nicknames :loading)
  (:export
   :load-all-plugins
   :init-plugins
   :add-to-arglist))
(in-package :loading)

;;;; http://reference-error.org/2015/08/30/common-lisp-finding-all-functions-in-a-package.html
;;;; © Ha-Duong Nguyen 2016. Unless clearly stated,
;;;; all contents are released under the terms of the CC BY-SA v4.0 license.
(defun all-function-symbols (package-name)
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fboundp symb)
                             (eql (symbol-package symb) package))
                    (push symb lst)))
                lst)
               (t
                (error "~S does not designate a package" package-name))))))

(defun load-all-plugins (plugins)
  "Runs init on all of the plugins in plugin-list"
  (loop for y in plugins do
    (progn
      (load (concatenate 'string globals:+plugin-folder+ "/" (symbol-name y) ".lisp"))
      (loop for x in (all-function-symbols y)
	    do (if (string= x 'init)
		   (setf globals:*command-list*
			 (funcall x globals:*command-list*)))))))

(defun init-plugins ()
  "Read from the index file into a list of plugins to use"
  (with-open-file (in globals:+plugin-file+)
		  (with-standard-io-syntax
		   (read in))))

(defmacro add-to-arglist (arglist &rest items)
  "A deceptively simple macro to simplify adding large numbers of commands.
   Also available to plugins."
  `(progn ,@(mapcar #'(lambda (x) `(push ,x ,arglist)) items)))
