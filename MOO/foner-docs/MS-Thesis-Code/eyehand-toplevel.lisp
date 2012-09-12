;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Eyehand -*-

(in-package :eyehand)

(defun INIT-EVERYTHING ()
  (schema::clear-item-class-cache)
  (init-world)
  (initialize-schema-mechanism))

;;;; Toplevel functions.

(def-microworld-toplevel SCHEMA::RUN-EYEHAND	; In SCHEMA: because that's usually my reader package, rather than some particular microworld.
  *number-of-actions*
  'init-everything
  #'eyehand-show-items-verbose
  #'clock-tick
  :log-name-prefix-default "eyehand-")

(defun SCHEMA::SAVE-EYEHAND (&optional (comment ""))
  (snapshot-world-state #'safe-copy-eyehand-state comment))

(defun SCHEMA::RESTORE-EYEHAND (&optional snapshot)
  (restore-world-state #'safe-restore-eyehand-state snapshot))

;;; End of file.
