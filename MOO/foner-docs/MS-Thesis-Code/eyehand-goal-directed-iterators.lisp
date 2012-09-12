;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Actual iterators that are specific to the EYEHAND world, and that are also
;;;; goal-directed.

(in-package :schema)

;;; &&& The uses of LAZY-COMPILE here might be stupid.  I wonder if they could just
;;; be #'(lambda () ...) ?  Might actually be _more_ efficient!  (Since then things like
;;; ALL-VF-COARSE-FOVEAL-ITEM-NUMBERS would be inlined.)  The problem would be
;;; if such things weren't defined until runtime (e.g., after JUST-INIT or whatever).

;;; +++ Allow scanning the eye for a general object.
;;; +++ Something else will determine when to stop.
(defgoal-eyehand GENERAL-SCAN
		 :percepts (make-bit-vector-from-list
			     (all-vf-and-vp-item-numbers))
		 :actions (with-action-selection
			    (maybe-cache-eye-translations)))

;;; To be used with something else, e.g., GENERAL-SCAN, since it establishes
;;; no percepts and no permitted actions.
(defgoal-eyehand GENERAL-SCAN-UNTIL-IN-FOVEA
		 :concurrent (general-scan)
		 :next high-res-scan-into-dead-center
		 :win (lazy-compile
			(some #'item-name-or-number-on?
			      (all-vf-coarse-foveal-item-numbers)))
; 		 :win-is-final t
		 )

;;; See comments in --GOAL-RAMBLING.LISP.  I'm unhappy with this.
(defgoal-eyehand GENERAL-SCAN-UNTIL-IN-FOVEA-WIF
		 :concurrent (general-scan)
		 :next high-res-scan-into-dead-center
		 :win (lazy-compile
			(some #'item-name-or-number-on?
			      (all-vf-coarse-foveal-item-numbers)))
 		 :win-is-final t
		 )

;;; +++ Wait a random amount of time, performing random actions.
;;; The idea here is to flail around and let us wander from some "stuck state"
;;; in which we couldn't find a goal that could succeed.  (This also lets the world
;;; do some random stuff, too.)

;;; We wait a random number of clock ticks whose average wait is this number.
;;; (If I wanted this to be deterministic, I could use two variables---a limit and a
;;; counter---and count the counter down against the limit.  Or I could just make
;;; a closure, of course...)
(defparameter *GOAL-RANDOM-WAIT-TIME* 20)

;;; &&& PROBLEM!  How do we know what the next goal is???
(defgoal-eyehand WAIT-A-RANDOM-TIME
		 :win (lazy-compile
			(zerop (random *goal-random-wait-time*)))
		 :actions (with-action-selection
			    (maybe-cache-all-actions)))

(defgoal-eyehand WAIT-A-RANDOM-TIME-THEN-SCAN
		 :next general-scan-until-in-fovea
		 :win (lazy-compile
			(zerop (random *goal-random-wait-time*)))
		 :actions (with-action-selection
			    (maybe-cache-all-actions)))

;;; +++ Checking to see if some object anywhere in the foveal region is the right one.

;;; Bound to a function which returns T if the object we're looking for is in the fovea.
;;; Part of IDENTIFY and similar things that must look for a particular target.
(defvar *FOVEA-MATCH-TARGET* nil)

(defun FOVEA-MATCH? ()
  (funcall *fovea-match-target*))

;;; Scan around in the fovea, at high resolution, as long as we've got something there.
;;; The idea here is to use GENERAL-SCAN-UNTIL-IN-FOVEA to get us to this state, and
;;; then to use this to build up some foveal items.  (OF course, if we're using selective
;;; attention that doesn't look at non-changed items, this won't buy us much, will it?
;;; Hmm.  But it _does_ allow _that_ mechanism some purchase, since, if we're pruning
;;; for goals, we're never even attending to any possibly-changed items there at all
;;; until this switches on the high-res stuff.  Yeah, that's it.)
;;;
;;; We stop when the object is centered in the fovea.  We could have done this at low-res,
;;; but this allows us to actually use the fovea to build up some schemas for later use.  From
;;; here, we go on to identify it.
(defgoal-eyehand HIGH-RES-SCAN-INTO-DEAD-CENTER
		 :concurrent general-scan	; Enable VF & VP percepts & all eye motions.
		 :next identify-on-fovea-without-scanning
		 :win (lazy-compile		; No LOSE, hence stick in this state until the thing's center.  Hmm.  If it drifts out completely?
			(item-name-or-number-on? (in-eyehand 'vf22)))
 		 :win-is-final t
		 :percepts (make-bit-vector-from-list
			     ;; This could just as well be ALL-EYE-ITEM-NUMBERS, since it equals VF & VP & ALL-FINE.
			     (all-fine-fovea-item-numbers)))

;;; If we fail, then we back off for a random amount of time.
(defgoal-eyehand IDENTIFY-ON-FOVEA-WITHOUT-SCANNING
		 :concurrent nil
		 :lose wait-a-random-time-then-scan
		 :next wait-a-random-time-then-scan	; Might as well start over in this case...  DBG.
		 :win #'true			; DBG.  Just win instantly.
 		 :win-is-final t
		 :percepts (make-bit-vector-from-list
			     (all-fine-fovea-item-numbers))
; 		 :actions nil)			; No scanning permitted---just win or lose.
		 :actions (with-action-selection	; DBG:  Just allow grasping, essentially a no-op.
			    (maybe-cache-hand-closures)))
; 
; ;;; +++ Finding some particular object
; 
; (defgoal-eyehand SCAN-THEN-IDENTIFY
; 		 :concurrent (general-scan)
; 		 :next #'identify-on-fovea-without-scanning
; 		 :achieved-p #'any-active-vf-coarse-foveal-items?
; 		 :percepts nil			; No additional ones except those in GENERAL-SCAN.
; 		 :actions nil)			; Ditto.
; 
; ;;; +++ Contacting objects.
; 
; (defgoal-eyehand GENERAL-HANDWAVING
; 		 :concurrent nil
; 		 :next nil
; 		 :achieved-p nil
; 		 :percepts (make-bit-vector-from-list
; 			     (all-hand-item-numbers)
; 
; (defgoal-eyehand HANDWAVE-UNTIL-ANY-OBJECT-CONTACT
; 		 :concurrent general-handwaving
; 		 :next nil
; 		 :achieved-p #'any-tact?
; 		 :percepts nil)

;;; End of file.
