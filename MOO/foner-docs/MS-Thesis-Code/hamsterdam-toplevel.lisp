;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Hamsterdam -*-

(in-package :hamsterdam)

;;; Note that there is no initialization here of the Hamsterdam microworld per se.
;;; WITH-GLOBAL-BUFFERS-AND-SOCKET, called by RUN-HAMSTERDAM, will initialize
;;; the network connection, and everything else that is required for microworld
;;; initialization happens on the C side of the link.
(defun INIT-EVERYTHING ()
  (schema::clear-item-class-cache)
  (initialize-schema-mechanism))

;;;; Toplevel functions.

(def-microworld-toplevel SCHEMA::RUN-HAMSTERDAM	; In SCHEMA: because that's usually my reader package, rather than some particular microworld.
  *number-of-actions*
  'init-everything
  #'ignore #'ignore				; No Hamsterdam status-reporting or clock-tick functions.
  :log-name-prefix-default "hamster-"
  :run-wrapper-form with-global-buffers-and-socket)

(defun SCHEMA::SAVE-HAMSTERDAM (&optional (comment ""))
  (snapshot-world-state #'ignore comment))	; There's no way to save the Hamsterdam state, but at least allow the schema state to be saved.

(defun SCHEMA::RESTORE-HAMSTERDAM (&optional snapshot)
  (restore-world-state #'ignore snapshot))	; Ditto for restoration.

;;; End of file.
