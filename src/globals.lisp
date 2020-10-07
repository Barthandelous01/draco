;;;; globals.lisp Copyright (C) 2020 Barthandelous01
;;;;
;;;; The global variables available to plugins.

(defpackage :barthandelous.globals
  (:use :cl)
  (:export
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

(defparameter +config-folder+ (concatenate 'string (uiop:getenv "HOME") "/.draco")
  "The config folder that draco's config and plugins live in")

(defparameter +plugin-folder+ (concatenate 'string +config-folder+ "/plugins")
  "The folder where draco's plugins live")

(defparameter +plugin-file+ (concatenate 'string +config-folder+ "/index.lisp")
  "The file that the list of plugins lives in")

(defparameter +binary-folder+ (concatenate 'string +config-folder+ "/bin")
  "Where the live binaries live")

(defparameter +db-file+ (concatenate 'string +config-folder+ "/draco.db")
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
