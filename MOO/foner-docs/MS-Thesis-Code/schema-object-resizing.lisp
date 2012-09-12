;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Resizing various data structures.  Not sure if this is temporary or not.

;;; The right way to do this is to remember (via some special form, probably)
;;; which objects use arrays that are dependent upon which limits.  For the moment,
;;; this is hardwired, though.

;;; +++
;;; Caveats:
;;;
;;; None of these update the :TYPE declarations, if any.  I hope that Genera, at least,
;;; just ignores 'em anyway.
;;;
;;; At the moment, this file contains exactly and only the forms I needed to update
;;; things after changing particular limits.  The general solution comes later.
;;;
;;; It just so happens that unknown states are 0, so that's a reasonable default for
;;; extending arrays.  It's also fortunate that the arrays were fixnum arrays, not
;;; arbitrary arrays, so that they get extended with 0 and not NIL.
;;; ---

(in-package :schema)

;;; Runs the body iff NEW-LIMIT is greater than the old array length.
;;; Does nothing if they're equal.  Errs if the new limit is shorter.
(defmacro UPDATE-CHECK-BOUNDS (array new-limit &body body)
  `(let ((length (length ,array)))
     (cond ((< length ,new-limit)
	    ,@body)
	   ((= length ,new-limit)
	    nil)
	   ((> length ,new-limit)
	    (error "~S has length ~D, but we wanted to extend it to ~D."
		   ,array length ,new-limit)))))

(defmacro UPDATE-ONE-SLOT (object slot new-length)
  `(update-check-bounds (,slot ,object) ,new-length
     (setf (,slot ,object) (adjust-array (,slot ,object) ,new-length))))

(defmacro UPDATE-SLOTS (object slots new-length)
  `(progn
     ,@(loop for slot in slots
	     collect `(update-one-slot ,object ,slot ,new-length))))

(defun UPDATE-SCHEMA-FOR-NEW-*FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-FLAGS* (schema)
  (update-slots schema (schema-result-conj-children)
		*fixna-required-to-hold-all-conj-flags*))

(defun UPDATE-SCHEMA-FOR-NEW-*FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-COUNTERS* (schema)
  (update-slots schema (schema-extended-result-conj-pos)
		*fixna-required-to-hold-all-conj-counters*))

(defun UPDATE-ALL-SCHEMAS-FOR-NEW-*CONJ-MAXIMUM* ()
  (loop for i from 0 below *schema-number*
	for schema = (get-schema i)
	do (update-schema-for-new-*fixna-required-to-hold-all-conj-flags* schema)
	   (update-schema-for-new-*fixna-required-to-hold-all-conj-counters* schema)))

(defun UPDATE-CONJ-FOR-NEW-*FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-FLAGS* (conj)
  (update-slots conj (conj-pos-flag-array
		      conj-neg-flag-array
		      conj-inclusion-array)
		*fixna-required-to-hold-all-conj-flags*))

(defun UPDATE-ALL-CONJS-FOR-NEW-*CONJ-MAXIMUM* ()
  (loop for i from 0 below *conj-number*
	for conj = (get-conj i)
	do (update-conj-for-new-*fixna-required-to-hold-all-conj-flags* conj)))

(defun UPDATE-ALL-OBJECTS-FOR-NEW-*CONJ-MAXIMUM* ()
  (declare (special *reliable-conj* *accessible-conj-pos* *predicted-result-conjs* *predicted-results-conjs-size*))
  (update-all-schemas-for-new-*conj-maximum*)
  (update-all-conjs-for-new-*conj-maximum*)
  (setf *reliable-conj*           (adjust-array *reliable-conj*          *fixna-required-to-hold-all-conj-flags*))
  (setf *accessible-conj-pos*     (adjust-array *accessible-conj-pos*    *fixna-required-to-hold-all-conj-flags*))
  (setf *predicted-result-conjs*  (adjust-array *predicted-result-conjs* *predicted-results-conjs-size*))
  (setf *conj-array*              (adjust-array *conj-array*             *conj-maximum*))
  )

;;; Don't need this one.
; (defun UPDATE-SCHEMA-FOR-NEW-*FIXNA-REQUIRED-TO-HOLD-ALL-ITEM-STATES* (schema)
;   (update-slots schema (schema-context-array
;                       schema-context-children
;                       schema-result-children)
;               *fixna-required-to-hold-all-item-states*))

;;; End of file.
