;;;; globals.lisp Copyright (C) 2020 Barthandelous01
;;;;
;;;; The global variables available to plugins.

(defpackage :barthandelous.globals
  (:use :cl)
  (:export
   :+config-folder+
   :*current-file*
   :*db*
   :*command-list*
   :+plugin-folder+
   :+binary-folder+  ;bin may be unnecessary
   :+plugin-file+
   :+prompt+
   :*current-filename*
   :+db-file+)
  (:nicknames :globals))
(in-package :globals)

(defparameter +config-folder+ (merge-pathnames
			       (make-pathname :directory '(:relative ".draco"))
			       (make-pathname :directory (uiop:getenv "HOME")))
  "The config folder that draco's config and plugins live in")

(defparameter +plugin-folder+ (merge-pathnames
			       (make-pathname :directory '(:relative "plugins"))
			       +config-folder+)
  "The folder where draco's plugins live")

(defparameter +plugin-file+ (merge-pathnames
			     (make-pathname :name "index" :type "lisp")
			     +config-folder+)
  "The file that the list of plugins lives in")

(defparameter +binary-folder+ (merge-pathnames
			       (make-pathname :directory '(:relative "bin"))
			       +config-folder+)
  "Where the live binaries live")

(defparameter +db-file+ (merge-pathnames
			 (make-pathname :name "draco" :type "db")
			 +config-folder+)
  "The file where the sqlite db lives")

(defparameter +prompt+ " $ "
  "The prompt for the CLI")

(defvar *current-file* ""
  "This is the currently open file. Used by plugins to run commands on
   the current file being analized.")

(defvar *db* nil
  "The live db.")

(defvar *current-filename* ""
  "The name of the current project")

(defvar *command-list* '()
  "The list of commands that the cli checks for.")
