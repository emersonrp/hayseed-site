;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Mudworld -*-

(in-package :mudworld)

;;; +++
;;; This stuff works okay in Genera, but gives warnings in Harlequin.  So break it out.
;;; Non-Genera systems don't have patch systems anyway.  Geez, I hate this solution!

;;; A function so additions to it are more easily noticed by the patch system and me.
#+Genera
(defun MUDWORLD-SETUP ()
  ;; Make the predicates required to know where we are, so we can tell if we've moved.
  (make-in-room?-predicates)
  ;; Make all the actions that try to move us.
  (make-all-room-motion-forms)
  ;; Wire up a topology.
  (wire-some-rooms)
  )

#+Genera
(eval-when (load eval)
  (mudworld-setup)
)

;;; ...

#-Genera
(make-in-room?-predicates)
#-Genera
(make-all-room-motion-forms)
#-Genera
(wire-some-rooms)

;;; ---

(def-microworld-toplevel SCHEMA::RUN-MUDWORLD	; In SCHEMA: because that's usually my reader package, rather than some particular microworld.
  *number-of-actions*
  'initialize-schema-mechanism
  #'ignore #'ignore				; No mudworld status-reporting or clock-tick functions.
  :log-name-prefix-default "mud-")

(defun SCHEMA::SAVE-MUDWORLD (&optional (comment ""))
  (snapshot-world-state #'safe-copy-mudworld-state comment))

(defun SCHEMA::RESTORE-MUDWORLD (&optional snapshot)
  (restore-world-state #'safe-restore-mudworld-state snapshot))

;;; End of file.
