;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Mudworld -*-

(in-package :mudworld)

;;;; General idea.

;;; We make a digraph of irregular rooms, a la a Mud, not a grid.
;;; Each room has predicates (true t or nil predicates, as is true of all
;;; the schema predicates so far) defined that can tell us:
;;;  . are we in a particular room
;;;  . is a particular exit direction valid
;;;  . [anything else?]
;;; Each room has actions that move us through an exit, if that exit exists.
;;; If it doesn't, nothing happens.  Actions don't return useful values.

;;; Make some crucial macros.
(def-microworld MUDWORLD)

;;;; Basic rooms.

(defparameter *ROOM-EXIT-DIRECTIONS* '(:east :west :north :south :up :down))

(defstruct ROOM
  (exit-east nil)
  (exit-west nil)
  (exit-north nil)
  (exit-south nil)
  (exit-up nil)
  (exit-down nil))

(defparameter *NUMBER-OF-ROOMS* 4)

(defun MAKE-SOME-ROOMS ()
  ;; Eventually, this will hook rooms together in some way, too.
  (loop repeat *number-of-rooms*
	collect (make-room)))

(defvar *ROOM-ARRAY*
	(make-array *number-of-rooms*
		    :initial-contents (make-some-rooms)))

(defvar *CURRENT-ROOM* 0)			; The number of the room we're currently in.

;;;; Primitive items.

;;; All predicates called by the schema mechanism must take zero arguments.
;;; We could make a zillion little closures, but we might as well make a zillion
;;; little functions instead---it'll probably run faster, especially in non-Genera lisps.
(defsubst MAKE-IN-ROOM?-NAME (number)
  (format nil "IN-ROOM-~D?" number))

(defmacro MAKE-IN-ROOM?-PREDICATES ()
  `(progn
     ,@(loop for counter from 0 below *number-of-rooms*
	     for predicate-name = (intern (make-in-room?-name counter))
	     collect `(defitem ,predicate-name
			(= ,counter *current-room*)))))

;;;; Actions.

(defsubst MAKE-ACTION-NAME (room-number direction)
  (format nil "~:@(ROOM-EXIT-~D-~A~)"		; E.g., take the DIRECTION exit from room number ROOM-NUMBER.
	  room-number direction))

(defsubst MAKE-ACTION-ACCESSOR-NAME (direction)
  (format nil "~:@(ROOM-EXIT-~A~)"
	  direction))

;;; All actions take zero args as well.  Same deal.
(defun MAKE-ROOM-MOTION-FORM (room-number direction)
  ;; Call this with a number and a direction, e.g., :EAST.
  (let ((action-name
	  (intern (make-action-name room-number direction)))
	(accessor-name
	  (intern (make-action-accessor-name direction))))
    `(defaction ,action-name
       (let ((exit (,accessor-name (aref *room-array* ,room-number))))
	 (when exit
	   (setf *current-room* exit))))))

(defun MAKE-ALL-ROOM-MOTION-FORMS-1 ()
  (loop for counter from 0 below *number-of-rooms*
	append (loop for dir in *room-exit-directions*
		     collect (make-room-motion-form counter dir))))

(defmacro MAKE-ALL-ROOM-MOTION-FORMS ()
  `(progn
     ,@(make-all-room-motion-forms-1)))

(defun WIRE-SOME-ROOMS ()
  ;; Totally by hand at the moment.  Assumes 4 rooms.
  (setf (room-exit-east  (aref *room-array* 0)) 1)
  (setf (room-exit-south (aref *room-array* 1)) 2)
  (setf (room-exit-west  (aref *room-array* 2)) 3)
  (setf (room-exit-north (aref *room-array* 3)) 0))

;;;; Saving and restoring the state of the microworld.  Trivial!

(defun SAFE-COPY-MUDWORLD-STATE ()
  (list *current-room*))

(defun SAFE-RESTORE-MUDWORLD-STATE (snapshot)
  (assert (= (length snapshot) 1))
  (setf *current-room* (car snapshot)))

;;; End of file.
