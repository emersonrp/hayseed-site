;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Support for iterators that are specific to the EYEHAND world,
;;;; and that are also goal-directed.

;;; Everything here must be run inside WITH-CURRENT-MICROWORLD.

(in-package :schema)

;;;; Use the bit of abuse defined in ACTION-SELECTION (based on ITEM-SELECTION)
;;;; to define particular groups of handy actions for the eyehand world.

;;; Utility.
(defsubst-exported MAKE-BIT-VECTOR-FROM-LIST (list &optional (bv (make-maximal-bit-vector)))
  (let ((bv bv))				; Stupid Genera compiler.
    #+Genera (declare (sys:array-register bv))
    (loop for elt in list
	  do (setf (bit bv elt) 1))
    bv))

;;; Return a bitvector of just these actions.
(defun CACHE-EYEHAND-AND-GIVE-BITVECTOR (key names &optional (pkg 'eyehand))
  (with-action-selection
    (maybe-cache-arbitrary key (repackage names pkg))
    (maybe-cache-numbers-for-class-named key)
    (make-bit-vector-from-list
      (get-basic-item-class-named
	(class-name-to-numbered-class-name key)))))

;;; Stick things in the right package.  I need this all over the damned place, because
;;; I'm using packages to differentiate things.  *sigh*

(defun IN-EYEHAND (symbol-or-list)
  (repackage symbol-or-list 'eyehand :numbers-okay t))

;;; These functions, like, e.g., ALL-VF-ITEM-NUMBERS, stay in the SCHEMA: package
;;; and are not exported.  It's bad enough that ALL-VF-ITEM-NUMBERS and its ilk are
;;; already in SCHEMA:; let's not export them, or these, and compound the damage.
;;; At least this way, I'll be able to shadow them in some third package (e.g., PUPPET:
;;; maybe?) if I have to.

(defun MAYBE-CACHE-EYE-TRANSLATIONS (&key (key 'eye-trans))
  (cache-eyehand-and-give-bitvector key '(eyef eyeb eyer eyel)))

(defun MAYBE-CACHE-HAND-TRANSLATIONS (&key (key 'hand-trans))
  (cache-eyehand-and-give-bitvector key '(handf handb handr handl)))

(defun MAYBE-CACHE-HAND-CLOSURES (&key (key 'hand-closures))
  (cache-eyehand-and-give-bitvector key '(grasp ungrasp)))

(defun MAYBE-CACHE-HAND-GESTURES (&key (key 'hand-gestures))
  (cache-eyehand-and-give-bitvector key '(handf handb handr handl grasp ungrasp)))

(defun MAYBE-CACHE-ALL-ACTIONS (&key (key 'all-actions))
  (cache-eyehand-and-give-bitvector
    key
    (mapcar #'intern-up (all-action-names))))

(def-post-microworld-init EYEHAND::FILL-EYEHAND-ACTION-CLASS-CACHE ()	; Must be in EYEHAND:.
  (maybe-cache-eye-translations)
  (maybe-cache-hand-translations)
  (maybe-cache-hand-closures)
  (maybe-cache-hand-gestures)
  (maybe-cache-all-actions))

;;; Useful macro.

(defmacro DEFGOAL-EYEHAND (name &key concurrent next lose win schemas-final percepts actions
			   win-is-final)
  (assert (not (and schemas-final win-is-final)))
  (with-current-microworld 'eyehand::eyehand
    `(defgoal ,(in-eyehand name)
	      :concurrent    (in-eyehand ',concurrent)
	      :next          (in-eyehand ',next)
	      :lose          (in-eyehand ',lose)
	      :win           ,win
	      :schemas-final ,(if win-is-final
				  win
				  schemas-final)
	      :percepts      ,percepts
	      :actions       ,actions)))

;;; End of file.
