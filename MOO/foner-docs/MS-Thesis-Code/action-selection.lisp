;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Selecting which action to take next.

(in-package :schema)

;;; Action selection is an essential part of any nonrandom experimental strategy.
;;; This particular implementation is still extremely simple, because composite
;;; actions aren't implemented yet.

;;; Note that the stuff on this particular page is not yet finished or hooked into anything.

;;; +++
;;; Hooks needed:
;;;    Number of actions.
;;;       [*NUMBER-OF-ACTIONS*, package specific!!---defined in each -TOPLEVEL.LISP.]
;;;    Action we took last iteration.
;;;       [Either we remember it ourselves, or get it from (CAR *ACTIONS-THIS-RUN*),
;;;       defined in DEF-MICROWORLD-TOPLEVEL.LISP.]
;;;    Number of schemas generated last iteration.
;;;       [Compute by differencing *SCHEMA-NUMBER* from last & current iteration.]
;;;    Callback for next action to take (so RUN-ONE-ITERATION-1 can call it).
;;;       [Provide a function; RUN-ONE-ITERATION-1 in DEF-MICROWORLD-TOPLEVEL.LISP]
;;; Given that DEF-MICROWORLD-TOPLEVEL.LISP already handles things by using stuff
;;; like MICROWORLD-CLOCK-TICK, should probably just define a similar hook.  This allows
;;; me to put the hook(s) in the microworlds (after DMT), and this file before DMT (like
;;; the rest of the schema & item-selection mechanism).
;;;
;;; Internals:  array of spinoffs/action.  exp decay average.
;;; ---

;;; Contains hardwired knowledge of what symbol is used by DEF-DEFACTION.
(defsubst CURRENT-MICROWORLD-NUMBER-OF-ACTIONS-SYMBOL ()
  (intern "*NUMBER-OF-ACTIONS*"	; &&& It's a pity that INTERN-SOFT & INTERN-LOCAL-SOFT are Genera-isms.
	  (current-microworld-package)))

(defsubst CURRENT-MICROWORLD-NUMBER-OF-ACTIONS ()
  (symbol-value (current-microworld-number-of-actions-symbol)))

;;; Currently dependent upon the *ACTIONS-THIS-RUN* vector metering counter,
;;; simply because it already exists and takes care of clearing itself at the start of a run.
(defsubst LAST-ACTION-TAKEN ()
  (declare (special *actions-this-run*))
  (car *actions-this-run*))

;;; A meter so it'll get cleared at the beginning of a run.
(def-scalar-metering-counter *SAVED-SCHEMA-NUMBER* 0)

(defun NUMBER-OF-SCHEMAS-GENERATED-LAST-ITERATION ()
  (prog1
    (- *schema-number* *saved-schema-number*)
    (setf *saved-schema-number* *schema-number*)))

#||
idea for counter-based exploration (e.g., nor more random action selection!):

find all applicable schemas (where I'm using that to mean, "those with
satisfied contexts"---check to make sure that's really what it means).
partition them by action.  add (average?) up the reliabilities of each
partition.  take the action with the lowest combined reliability.  That
will tend to explore parts of the state space that we haven't seen
before.

This is kinda like using a competence map, because we're going where we
think the knowledge is poorest, rather than the last-visited-state or
whatever.

Note that the failures of Thrun's theorems 1-3 in stochastic domains
probably doesn't kill us here.  It is at least a finite domain...
||#

;;;; Abuse the item-selection mechanism to generate subsets of actions instead of subsets of items.

;;; See the comments at the relevant variables.  This is all a grotendous kluge
;;; that shouldn't have been done this way, and should be rewritten someday.

;;; The variable below is part of a gross kluge.  It's set by DEFACTION so it can
;;; be used in ACTION-SELECTION, which itself is abusing the ITEM-SELECTION mechanism
;;; to generate subsets of actions instead of subsets of items.  See the comments in
;;; those files for more details.
;;; 
;;; This is here because NUMBER-OF-ITEM-NAMED assumes it's getting handed
;;; things with look like Items (e.g., no generic accessor), and because the ordinary
;;; *ACTION-ARRAY* holds only compiled function objects, with no names, and there's
;;; no portable (e.g., non-Genera) way of getting the function name from the compiled
;;; function object.  So we keep a parallel array around full of "fake" items (e.g., they've
;;; only got non-NIL NAME and CODE slots) so we can hand _that_ to NUMBER-OF-ITEM-NAMED.
;;; Has this gotten gross yet?
;;;
;;; We don't put this in the EYEHAND (or whatever) package automatically, as DEF-DEFACTION
;;; does, because the original *ACTION-ARRAY* or *ITEM-ARRAY* don't have one per package either.
;;; This is a holdover from the original [Ramstad] implementation, and this dichotomy of where
;;; things are is perpetuated here, too.  [&&&&&Um, so how do we properly initialize this for
;;; different microworlds?  Re-remember how I did this...]
(defvar *ACTION-ARRAY-CAST-AS-ITEM-ARRAY* nil)

;;; This makes a "fake" item that's really an action.
;;; It also sets everything but the name & compiled function object NIL,
;;; so we'll explode if we touch it, and so save consing those large arrays.
(defun MAKE-ACTION-CAST-AS-ITEM (action-name action-function)
  (make-item-internal
    action-name action-function
    nil nil nil nil nil nil nil))

;;; Must be called inside the context of WITH-CURRENT-MICROWORLD and WITH-ACTION-SELECTION.
(defun STUFF-*ACTION-ARRAY-CAST-AS-ITEM-ARRAY* ()
  (unless *action-array-cast-as-item-array*
    (setf *action-array-cast-as-item-array*
	(make-array *action-maximum* :element-type 'compiled-function))
    (loop for action in (reverse (current-microworld-primitive-items))
	  for counter from 0
	  do (setf (aref *action-array-cast-as-item-array* counter)
		   (make-action-cast-as-item
		     (symbol-name action)
		     ;; Notice that this is SCHEMA:*ACTION-ARRAY*, and that we've got a one-to-one
		     ;; correspondence between positions in *ACTIONS* (reversed!  because it was
		     ;; created with PUSHNEW!) and positions in *ACTION-ARRAY*, because DEFACTION
		     ;; built them both in parallel.
		     (aref *action-array* counter))))))

;;; Must be called inside the context of WITH-CURRENT-MICROWORLD.
(defmacro-exported WITH-ACTION-SELECTION (&body body)
  `(let ((*item-class-cache-name* "*ACTION-CLASS-CACHE*")
	 (*primitive-items-symbol-name* "*ACTIONS*")
	 (*item-array-symbol-name* (intern "*ACTION-ARRAY-CAST-AS-ITEM-ARRAY*"))
	 (*item-symbol-name-table-name* (intern "*ACTION-SYMBOL-NAME-TABLE*")))
     (stuff-*action-array-cast-as-item-array*)	; Make sure it's initialized.
     ,@body))

;;; Debugging funtion.
; (defun TEST-WAS (class)
;   (with-current-microworld 'eyehand::eyehand
;     (with-action-selection
;       (with-basic-item-names-and-numbers-of-class (names numbers class)
; 	(format t "~&~S = ~S, ~S = ~S~&"
; 		'names names
; 		'numbers numbers)
; 	(maybe-cache-eye-actions)
;         (break)))))

;;; End of file.
