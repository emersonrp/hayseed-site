;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Adding goals for focus.

(in-package :schema)

;;; Concurrent goals run at the same time as this goal.  We OR in their percept &
;;; action sets, and we consider this goal satisfied if any concurrent goal is satisfied
;;; (e.g., an OR, not an AND).  Goals may be cyclic; we'll deal.

;;; This is haired up by the fact that, until a microworld has been initialized, the
;;; various cached crap created by ITEM-SELECTION.LISP can't be set up (since it
;;; looks at the current microworld state).  _That_ means that we can't actually fill
;;; in the various bitvectors until after such initialization, which means we have to
;;; essentially cache & check at runtime.  Feh.  Note that we only cache _once_, so
;;; if you change the mapping of percepts or actions to their numbers, you'll have to
;;; clear it (by recompiling the DEFGOAL).  This is reasonably unlikely, of course.
;;; And, of course, this would be easier if I was using CLOS for the goals, since
;;; then the accessors could check.

;;; Deciding whether a goal is satisfied, and which goal to run next in either case,
;;; is a little tricky.  Here's how it works.  Every goal has a WIN slot.  We funcall this
;;; with no args.  If it returns non-NIL, the goal has succeeded (e.g., it's satisfied).
;;;
;;; Once we've decided whether the goal is satisfied or not, we have two choices.  If
;;; the goal was satisfied, then we transition to the goal named in the NEXT slot for
;;; this goal.  If the goal was NOT satisfied, then we remember the goal named in the
;;; LOSE slot, and we try any other concurrent goals that may exist.  (Note that the
;;; order in which we try concurrent goals is undefined.)  If none of them succeed,
;;; then we pick the next goal randomly from the goals in the LOSE slots.  If none of
;;; the concurrent goals had any LOSE goals defined, then we stick in the current
;;; goal.  (This allows us to define a goal that we cannot come out of, assuming no
;;; concurrent goals are being run with it, by failing to define anything for the LOSE
;;; slot.  On the other hand, we can avoid getting stuck by making sure that at least
;;; one concurrent goal has this defined.)
;;;
;;; Obviously, this implements a little FSM, or, if you prefer, a simple rule system.

;;; GOAL NAME below means some symbol in the right package.
(defstruct (GOAL)
  (name)					; GOAL NAME:  Name of this goal.
  (concurrent)					; GOAL NAME(s):  Which other goals to pursue at the same time.
  (next)					; GOAL NAME:  Which other goal (singular!) to pursue next.
  (lose)					; GOAL NAME:  Goal to (maybe) transition to if we don't succeed.
  (win)						; FUNCTION:  Called to determine success (learning).
  (schemas-final)				; FUNCTION:  Called to determine membership in FINAL (chaining to goals).
  (percepts)					; BITVECTOR of which items we should attend to.
  (actions)					; BITVECTOR of which actions are allowable.
  (marked)					; [INTERNAL boolean]  For recursive walks.
  (initialized))				; [INTERNAL boolean]  Set to T once we've cached the runtime values.

(defvar *ALL-GOALS* nil)

;;; This might as well also be a function, because it's called from lots of other
;;; functions that take a fair bit of time to run, and it's not called often
;;; anyway---basically, only when it's time to switch goals.
(defun FIND-GOAL (goal-name &optional nonexistent-okay)
  (let ((goal (find goal-name *all-goals* :key #'goal-name)))
    (unless (or goal nonexistent-okay)
      (error "Couldn't find goal named ~A." goal-name))
    goal))

;;; Useful if you're gonna be talking about one goal a lot in some function.
(defmacro COERCE-NAME->GOAL (name-or-goal)
  `(unless (goal-p ,name-or-goal)
     (setf ,name-or-goal (find-goal ,name-or-goal))))

;;; Useful for a one-shot.
(defsubst NAME->GOAL (name-or-goal)
  (if (goal-p name-or-goal)
      name-or-goal
      (find-goal name-or-goal)))

;;; These guys get initialized dynamically.
(defvar *GOAL-PERCEPT-SET* nil)
(defvar *GOAL-ACTION-SET* nil)
(defvar *GOAL* nil)

(defmacro-exported DEFGOAL (name &key concurrent next lose win schemas-final percepts actions)
  `(let ((goal (make-goal
		 :name ',name			; Quite deliberately not a keyword:  This allows different goals for different packages.
		 :concurrent ',concurrent
		 :next ',next
		 :lose ',lose
		 :win ',win
		 :schemas-final ',schemas-final
		 :percepts ',percepts
		 :actions ',actions
		 :initialized nil)))
     ;; This is Genera's PUSHNEW :REPLACE T, essentially.
     (let ((old-goal (find (goal-name goal) *all-goals* :key #'goal-name)))
       (cond (old-goal
	      (setf *all-goals* (substitute goal (goal-name old-goal)
					    *all-goals* :key #'goal-name)))
	     (t
	      (push goal *all-goals*))))))

;;; If the goal isn't initialized, initialize it, else do nothing.  Don't call this until the
;;; world's been initialized!  There's no good way of knowing that this world
;;; initialization has taken place, but evaluation of the goal-percepts or goal-actions
;;; initializers will probably bomb if it hasn't happened yet.
;;;
;;; Note that the process of initializing a goal does NOT convert the names in the
;;; CONCURRENT or NEXT slots into actual functions:  first, because they're NOT
;;; actually functions, but only strucures stashed in *ALL-GOALS*, and second, because
;;; even if they were, I'd want to be able to recompile one and have others that refer
;;; to it pick up the changes.  So things which look at those slots had better be prepared
;;; to use FIND-GOAL on their contents.
(defun MAYBE-INITIALIZE-GOAL (goal)
  (unless (goal-initialized goal)
    (setf (goal-percepts goal)      (eval (goal-percepts goal)))
    (setf (goal-actions  goal)      (eval (goal-actions  goal)))
    (setf (goal-next goal)          (eval (goal-next goal)))
    (setf (goal-lose goal)          (eval (goal-lose goal)))
    (setf (goal-win  goal)          (eval (goal-win  goal)))
    (setf (goal-schemas-final goal) (eval (goal-schemas-final goal)))
    (setf (goal-concurrent goal)    (listify (eval (goal-concurrent goal))))
    (setf (goal-initialized goal) t)))		; Set this last, in case we bombed above.

;;; &&& $OPT:  I might want to have a global that indicates whether all goals
;;; have been inited, so we don't have to scan them with this, given that this
;;; may get called on each iteration of a performance evaluation (_not_ each
;;; iteration of learning).  I could do that by having DEFGOAL reset the variable.
;;; (And if I make some sort of INVALIDATE-ALL-GOALS function for whatever
;;; reason, it would have to invalidate that as well.)
(defun MAYBE-INITIALIZE-ALL-GOALS ()
  (mapc #'maybe-initialize-goal *all-goals*))

(defsubst MAKE-MAXIMAL-BIT-VECTOR ()
  (let ((size (max *schema-maximum* *item-maximum*)))
    (make-array size :element-type 'bit :initial-element 0)))

;;; Creates them iff they haven't already been created.  Doesn't touch them if
;;; they're already created.  This means that, if they've been created but are
;;; nonzero, we still leave them alone.  If you require that they be zero, then
;;; you should be using FORCE-INITIALIZE-GOAL-SETS instead.
(defun MAYBE-INITIALIZE-GOAL-SETS ()
  (maybe-initialize-all-goals)
  (unless *goal-percept-set*
    (setf *goal-percept-set* (make-maximal-bit-vector)))
  (unless *goal-action-set*
    (setf *goal-action-set*  (make-maximal-bit-vector)))
  (values))

;;; WITH-STARTING-GOAL-AND-GENERATOR calls SWITCH-TO-GOAL, which calls this, so
;;; there shouldn't be any need to make this a DEF-POST-MICROWORLD-INIT.
(defun FORCE-INITIALIZE-GOAL-SETS ()
  (maybe-initialize-goal-sets)			; Initialize goals, & create the bitvectors, iff they don't already exist.
  (clear-bit-vector *goal-percept-set*)		; If they already existed, make sure they're zeroed.
  (clear-bit-vector *goal-action-set*)		; Ditto.
  (values))

(defun CLEAR-GOAL-MARKS ()
  (loop for goal in *all-goals*
	do (setf (goal-marked goal) nil)))

;;; As a convenience, we allow NIL to act as an empty vector (one with no bits on,
;;; e.g., a cardinality of zero) when investigating the current goal's percepts or
;;; actions.
(defun WALK-GOAL-SETS-INTERNAL (goal set accessor)
  (unless (goal-marked goal)
    (setf (goal-marked goal) t)			; We've now visited this goal.
    (let ((more-bits (funcall accessor goal)))	; This goal's additional percepts or actions.
      (when more-bits				; This is a vector, hence probably additional percepts or actions.
	(bit-ior set (funcall accessor goal) t)))	; OR in whatever bits this goal talks about.
    (loop for concurrent-name in (goal-concurrent goal)
	  do (walk-goal-sets-internal (find-goal concurrent-name) set accessor))))

;;; The toplevel general routine for walking over concurrent goals.
(defun WALK-GOAL-SETS (goal set accessor)
  (clear-goal-marks)
  (walk-goal-sets-internal goal set accessor))

(defun WALK-GOAL-PERCEPTS (goal)
  (walk-goal-sets goal *goal-percept-set* #'goal-percepts))

(defun WALK-GOAL-ACTIONS (goal)
  (walk-goal-sets goal *goal-action-set* #'goal-actions))

;;; Unconditionally switches to a particular goal.
;;; Bashes the percept and action sets to the current goal.
(defun SWITCH-TO-GOAL (goal)
  (setf *goal* goal)
  (force-initialize-goal-sets)
  (walk-goal-percepts goal)
  (walk-goal-actions  goal))

;;;; Figuring out if the goal is satisfied:  Utilities.

;;; Utilities:  This aren't actually used here, but are available for use by calls to
;;; DEFGOAL etc.  Note that these particular utilities are used both when determining
;;; when a goal is satisfied (during learning), and when a goal should be in the FINAL
;;; set (during chaining to goals when demonstrating competence).  They have to do
;;; slightly different things in these two cases, though:  in the former, they must
;;; consult the _actual_ state of the world, whereas, in the latter, they must consult
;;; the _items in some particular schema's context array_.  Hence, the actual lookup
;;; function is conditionalized here, such that a LET can change it.  This lets us reuse
;;; the lexical appearance of the code in both relevant slots in the goal (the WIN slot
;;; and the SCHEMAS-FINAL slot), although the latter will have a
;;; WITH-SCHEMA-CONTEXT-ITEMS-AS-MICROWORLD-STATE wrapped around the call, to
;;; implement the necessary LET.
;;;
;;; Unfortunately, we cannot have both the flexibility to do it this way (reusing the
;;; lexical appearance of the code) _and_ use macros below for efficiency, because
;;; we'd wind up calling EVERY or SOME on macros, which aren't funcallable.  So
;;; we'll give up a little efficiency in an inner loop and do a FUNCALL inside
;;; ITEM-NAME-OR-NUMBER-IN-STATE?, which hurts worse on stock hardware than in
;;; Genera, of course.  Oh, well.

;;; The default case:  look in the microworld.  Unfortunately, GET-ITEM-CURRENT-STATE
;;; is a macro, not a subst, so we have to make it funcallable by hand.  *sigh*
(defparameter *GOAL-ITEM-CHECKER-FN* #'(lambda (item-number)
					 (get-item-current-state item-number)))

(defsubst SCHEMA-CONTEXT-AS-MICROWORLD-ITEM (item-number)
  (state-array-get-state
    (schema-context-array *goal-item-checker-current-schema*)
    item-number))

;;; Make sure that this is bound when actually running the function in SCHEMA-FINAL.
;;; [It's a poor idea to wrap this around the actual definition of the slot function,
;;; unless we're definitely not inheriting via :WIN-IS-FINAL, since otherwise it either
;;; doesn't take effect (if it surrounds a LAZY-COMPILE, since it's only a LET, not a
;;; COMPILER-LET), or, if it is itself surrounded by LAZY-COMPILE, it's not clear what
;;; the inner function is (e.g., (LC (WSCISMS (LC ...))) is poorly defined).]
(defmacro WITH-SCHEMA-CONTEXT-ITEMS-AS-MICROWORLD-STATE (&body body)
  `(let ((*goal-item-checker-fn* #'schema-context-as-microworld-item))
     ,@body))

;;; Helper special variable for use when we're using some schema as the "microworld state".
(defvar *GOAL-ITEM-CHECKER-CURRENT-SCHEMA* nil)

(defmacro WITH-CURRENT-SCHEMA-AS-CONTEXT-ITEMS-FOR-MICROWORLD-STATE (schema &body body)
  `(let ((*goal-item-checker-current-schema* ,schema))
     ,@body))

(defsubst ITEM-NAME-OR-NUMBER-IN-STATE? (item state)
  (let ((item-number
	  (etypecase item			; Blow up if this gets something random.
	    (number item)
	    (symbol (number-of-item-named item)))))
    (state-eq (funcall *goal-item-checker-fn* item-number) state)))

(defsubst ITEM-NAME-OR-NUMBER-ON? (item)
  (item-name-or-number-in-state? item (make-state-on)))

(defsubst ITEM-NAME-OR-NUMBER-OFF? (item)
  (item-name-or-number-in-state? item (make-state-off)))

;;; Each of ON or OFF can be a single item (name or number), or a list of them..  If
;;; every item in the ON list is asserted, and no items in the OFF list is asserted,
;;; then we consider the goal to be satisfied.  (Each element of one of these lists
;;; may be either the symbol naming an item, e.g., VF22, or the number of some
;;; item, e.g., 37.  We'll do the right thing in both cases.  The idea here is to allow
;;; either producing a list of numbers from one of the functions in
;;; ITEM-ITERATORS.LISP, or just specify a particular item if we know absolutely that
;;; that's the one we want.)
;;;
;;; Policy decision:  If both ON and OFF are null, we fail.  This allows leaving them
;;; both blank to mean, "Defer success to some other goal's judgment."  This also
;;; allows us to use a goal just to specify some permitted actions (e.g., by leaving
;;; these blank), and having it run concurrently with, e.g., some goal that might have
;;; only these slots, and no permitted actions.
(defsubst ITEMS-ON-AND-OFF? (on off)
  (and (or on off)
	(every #'item-name-or-number-on?  (listify on))
	(every #'item-name-or-number-off? (listify off))))

;;; ---

;;; This is just sugar.  It allows me to stick some function in the WIN slot of a goal
;;; that, when the goal is initialized, yields a compiled function (for speed).  This
;;; works because goal initialization calls the evaluator.  I could just as easily call
;;; COMPILE directly, or even define a real live function and just mention its name
;;; instead.  But this avoids having to make a zillion functions that each get used
;;; once.
(defmacro LAZY-COMPILE (&rest forms)
  `(compile nil #'(lambda () ,@forms)))

;;; Figuring out if the goal is satisfied:  This is what actually checks.
(defsubst GOAL-ACHIEVED? (goal)
  (when (goal-win goal)				; If there's no predicate, we can't win.
    (funcall (goal-win goal))))

(defvar *GOAL-LOSERS* nil)			; Bashed on each iteration.

(defun WALK-GOAL-ACHIEVED-INTERNAL (goal)
  (unless (goal-marked goal)
    (setf (goal-marked goal) t)			; We've now visited this goal.
    (or (goal-achieved? goal)
	(progn
	  (when (goal-lose goal)
	    (push (goal-lose goal) *goal-losers*))
	  (loop for concurrent-name in (goal-concurrent goal)
		thereis (walk-goal-achieved-internal
			  (find-goal concurrent-name)))))))

;;; Returns first value of T if the current goal, or some concurrent goal
;;; (recursively) is satisfied.  The second value is a list of possible losing goals, one
;;; of which we should switch to if the first value was NIL.
(defun WALK-GOAL-ACHIEVED? (goal)
  (setf *goal-losers* nil)
  (clear-goal-marks)
  (let ((achieved? (walk-goal-achieved-internal goal)))
    (values achieved?
	    *goal-losers*)))

;;;; The goal FSM.

;;; Given some current goal that we're pursuing, see if it's been satisfied.
;;; If it has, switch to the next goal, destructively clearing the percept and action
;;; sets and reinitializing them from the new goal, and return the new goal.  If it
;;; hasn't, just return the current goal again.

(def-debugging-switch REPORT-GOAL-TRANSITIONS t)	; Report if we have switched goals.
(def-debugging-switch REPORT-GOAL-PICK-LOSERS nil)	; Report if we pick a losing goal, because we couldn't satisfy the current goal.
(def-debugging-switch REPORT-GOAL-NULL-LOSERS nil)	; Report if we didn't have any losing goals to pick from, yet the current goal was unsatisfiable.

(defun GOAL-FSM (goal)
  (multiple-value-bind (achieved? losers)
      (walk-goal-achieved? goal)
    (let ((next-goal
	    (cond (achieved?
		   (report-goal-transitions-format
		     "~&~5D Goal ~A satisfied.  Switching to goal ~A.~&"
		     *clock-tick*
		     (goal-name goal)	; GOAL is a structure, not a name.
		     (goal-next goal))	; This slot already holds just a name.
		   (let ((new-goal (goal-next goal)))
		     (unless new-goal
		       (error "~5D Goal ~A succeeded, but had no next goal!"
			      *clock-tick*
			      (goal-name goal)))
		     (find-goal new-goal)))
		  (t
		   (cond ((null losers)
			  (report-goal-null-losers-format
			    "~&~5D Goal ~A wasn't satisfied, but there were no ~
                               losing goals to pick from.  Staying in goal ~:*~A.~&"
			    *clock-tick*
			    (goal-name goal))
			  goal)
			 (t
			  (let ((random-loser (find-goal (nth (random (length losers)) losers))))
			    (report-goal-pick-losers-format
			      "~&~5D Goal ~A wasn't satisfied.  Picking random loser ~A.~&"
			      *clock-tick*
			      (goal-name goal)
			      (goal-name random-loser))
			    random-loser)))))))
      (switch-to-goal next-goal)
      *goal*)))

;;;; Action selection.

;;; For callers that want to pick a different action selector.
;;; Note that ARGS is &OPTIONAL, _not_ &REST, for convenience of occasional callers.
;;; This means it had better be, e.g., '(1 2) [quoted] if you supply it.
(defmacro-exported WITH-MICROWORLD-ACTION-SELECTION-FUNCTION ((fn &optional args) &body body)
  `(let ((*microworld-action-selection-function* ,fn)
	 (*microworld-action-selection-function-extra-args* ,args))
     ,@body))

;;; Picks an action at random from among those enabled by the action set.
;;; This isn't terribly efficient (e.g., it conses).  So sue me.
(defun ALWAYS-GOAL-DIRECTED-RANDOM-ACTION-SELECTOR (&rest ignore)
  (let* ((actions (enumerate-bit-vector *goal-action-set*))
	 (length (length actions)))
    (when (zerop length)
      (error "Sorry, there aren't any permitted actions!"))
    (nth (random length) actions)))

(defmacro WITH-ALWAYS-GOAL-DIRECTED-RANDOM-ACTION-SELECTOR (&body body)
  `(with-microworld-action-selection-function ('always-goal-directed-random-action-selector)
     ,@body))

;;; This is a variant of the above which occasionally takes _any_ action, rather
;;; than only taking one of the set above.  Why might we want to do this?  Well,
;;; in almost-completely-static microworlds like the eyehand world, if we're, e.g.,
;;; only moving the eye, the hand will "stick" in place for the entire duration of
;;; learning.  This means that we'll learn all sorts of (wrong) things, such as that
;;; there's _always_ an object (e.g., the hand) at some particular point.  So we
;;; introduce enough randomness that this won't happen (e.g., the reliability of
;;; any schema predicting it will be low).
;;;
;;; Note that this randomness is only while _learning_, not while chaining to goals
;;; during performance evaluation.

;;; If a uniformly-distributed random number in [0,1), generated each iteration, is
;;; below this in any given iteration, ignore what GOAL-DIRECTED-RANDOM-ACTION-SELECTOR
;;; would recommend, and instead take an action completely at random (which might be one
;;; of its recommended actions, but we don't care---if it turns out that the set of permitted
;;; actions is so large that we can't sensibly set this value large enough to cause occasional
;;; deviations into the other set, _then_ I'll deal with making it do a strict set-difference,
;;; forcing it to take some other action.  But I'll do that with a slightly different name, so
;;; I can tell which runs were done which way.)
(def-reported-parameter *GOAL-DIRECTED-OCCASIONAL-RANDOMNESS-FACTOR* .1)

;;; NOTE!  This assumes that the default action-selector is the function RANDOM.
(defun USUALLY-GOAL-DIRECTED-RANDOM-ACTION-SELECTOR (number-of-actions &rest ignore)
  (if (< (random 1.0)				; NB!  1.0, _not_ 1!
	 *goal-directed-occasional-randomness-factor*)
      (random number-of-actions)
      (always-goal-directed-random-action-selector)))

(defmacro WITH-USUALLY-GOAL-DIRECTED-RANDOM-ACTION-SELECTOR (&body body)
  `(with-microworld-action-selection-function ('usually-goal-directed-random-action-selector)
     ,@body))

;;;; Composing generators and iterators.

;;; [For a previous, bad idea at this, consult GOALS-BAD-IDEAS.LISP.]

;;; This try is based on the idea of masking.  If we compose two arbitrary iterators,
;;; even if they yield the same values, if they yield them in inconsistent orders,
;;; we're in trouble, because we'd have to arbitrate that order.  Instead, we declare
;;; by caveat that one iterator gets to win any ordering dispute, and the other
;;; iterator has no input whatsoever into ordering.  This second, losing iterator is
;;; therefore called a "mask", and might cause certain values yielded by the first
;;; iterator to be ignoed, but cannot add values, nor can it reorder them.

;;; In this particular implementation of masks, the losing iterator is not even an
;;; iterator at all, but a simple bitvector (since order doesn't matter anyway, and
;;; since the things that tend to generate this loser generator bitvectors anyway).
;;; This bitvector is generally the percept set of some goal.

;;; Returns the two parts of a generator.  The YIELD function yields successive
;;; elements from the original generator, omitting those whose bit position in the
;;; mask-bitvector is zero, and the RESET function just calls the original resetter.
(defun MAKE-MASKED-GENERATOR (original-generator mask-bitvector)
  ;; Paranoia, from a weird bug I once had.  (If it's NIL, its length will be zero,
  ;; and we'll silently fail to generate anything.  Had I used the less-portable
  ;; ARRAY-LENGTH, it would have blown out immediately, and debugging this would
  ;; have been a little faster (first half of the weird bug would have been obvious).
  (assert (not (null mask-bitvector)))
  (with-iterator (yielder resetter original-generator)
    ;; I never mandated that one must call the resetter first, but I suppose
    ;; that there's no harm in doing so, just in case I blew it somewhere,
    ;; considering that this is the first time that anything but DO-ITERATOR
    ;; has been doing this job, and _it_ called the resetter first (even though
    ;; I comment elsewhere that I shouldn't depend on that).
    (resetter)
    ;; Now return the new yielder & resetter.
    (values
      #'(lambda ()				; YIELD.
	  (loop for yielded = (yielder)
		while yielded
		unless (or (>= yielded (length mask-bitvector))	; If we fell of the end, treat it like a zero.
			   (zerop (bit mask-bitvector yielded)))
		  do (return yielded)))
      #'(lambda ()				; RESET.
	  (resetter)))))

;;; Items are easy, since we're already given a bunch of item numbers from the goal stuff.
(defun MAKE-GOAL-DIRECTED-ITEM-PERCEPT-GENERATOR (item-generator &optional (percept-set-bv *goal-percept-set*))
  (make-masked-generator item-generator percept-set-bv))

;;; Schemas are trickier, since what we need is a list of those schemas dependent
;;; upon the items that we're given.  By "dependent", we mean both context and
;;; result, a la WITH-SOME-SCHEMAS, which is eventually called from
;;; ALL-BARE-SCHEMAS-PLUS-SCHEMAS-DEPENDENT-UPON-CHANGED-ITEMS-GENERATOR
;;; and was thus my hint that I wanted both context and result.  This rewrites some
;;; code to avoid consing intermediate lists (since the input & output are both bitvectors,
;;; yet WITH-SOME-SCHEMAS & its ilk would listify intermediate bunches of item numbers).
(defun MAKE-GOAL-DIRECTED-SCHEMA-PERCEPT-GENERATOR (schema-generator &optional (percept-set-bv *goal-percept-set*))
  (let ((mask (make-schema-bit-vector)))
     ;; Set all the schemas selected.  [Stolen from WITH-SOME-SCHEMAS.]
     (loop for item-index from 0 below (length percept-set-bv)
	   do (unless (zerop (bit percept-set-bv item-index))
		(let* ((item (get-item item-index))
		       (c (item-context-dependent-schemas item))
		       (r (item-result-dependent-schemas item)))
		  (bit-ior mask c t)	; T -> bash MASK.
		  (bit-ior mask r t))))	; Ditto.
     ;; Set all the bare schemas in our bit vector.  [Stolen from WITH-ALL-BARE-AND-SOME-OTHER-SCHEMAS.]
     (loop for i in (all-bare-schema-numbers)	; $OPT:  Precompute this at the start of a run, then not again.
	   do (setf (bit mask i) 1))
     ;; Now make the new generator.
     (make-masked-generator schema-generator mask)))

;;; &&& $OPT:  Granted, these are tiny functions, but wouldn't they run faster if
;;; they'd been compiled as well?  Of course, they're only each called once per
;;; clock tick, rather than in some inner loop, so perhaps this just doesn't matter.
;;; --- Foner 2 May 94.
(defmacro-definer DEF-GOAL-DIRECTED-PERCEPT-GENERATOR (original-generator-name-exp mask-generator-name
						       &optional (percept-set-bv '*goal-percept-set*)
						       (mask-name "MASKED"))
  (let* ((original-generator-name (eval original-generator-name-exp))
	 (masked-generator-name
	   (intern-format "~A-~A"
			  mask-name
			  original-generator-name)))
    `(progn
       (defun ,masked-generator-name ()
	 (,mask-generator-name ',original-generator-name ,percept-set-bv))
       ',masked-generator-name)))

(defmacro-definer DEF-GOAL-DIRECTED-ITEM-PERCEPT-GENERATOR (item-generator-name-exp
						            &optional (percept-set-bv '*goal-percept-set*)
							    (mask-name "MASKED"))
  `(def-goal-directed-percept-generator
     ,item-generator-name-exp make-goal-directed-item-percept-generator
     ,percept-set-bv ,mask-name))

(defmacro-definer DEF-GOAL-DIRECTED-SCHEMA-PERCEPT-GENERATOR (schema-generator-name-exp
						              &optional (percept-set-bv '*goal-percept-set*)
							      (mask-name "MASKED"))
  `(def-goal-directed-percept-generator
     ,schema-generator-name-exp make-goal-directed-schema-percept-generator
     ,percept-set-bv ,mask-name))

;;; Put this lexically inside, e.g., WITH-SPINOFF-SCHEMAS-WITH-RECENTLY-UPDATED-STATS-GENERATORS
;;; and WITH-UEIS-HISTORY-ITEMS-ALL-SCHEMAS-GENERATORS.  It rebinds the various variables used by
;;; DEF-ITERATOR-GENERATOR etc to point to new, masked iterators that implement goal direction.
;;; For the moment, all four of them get masked with the same goal mask; I'm not sure if this is the
;;; right idea or not.
(defmacro WITH-MASKED-GENERATORS (mask &body body)
  `(let ((*use-which-spinoff-schema-number-generator?*
	   (def-goal-directed-schema-percept-generator *use-which-spinoff-schema-number-generator?* ,mask))
	 (*use-which-spinoff-item-number-generator?*
	   (def-goal-directed-item-percept-generator   *use-which-spinoff-item-number-generator?*   ,mask))
	 (*use-which-ueis-schema-number-generator?*
	   (def-goal-directed-schema-percept-generator *use-which-ueis-schema-number-generator?*    ,mask))
	 (*use-which-ueis-item-number-generator?*
	   (def-goal-directed-item-percept-generator   *use-which-ueis-item-number-generator?*      ,mask)))
     ,@body))

;;; The action-selector used by WITH-STARTING-GOAL-AND-GENERATOR.
(def-reported-parameter *GOAL-DIRECTED-LEARNING-ACTION-SELECTOR* 'usually-goal-directed-random-action-selector)

;;; E.g., (WITH-STARTING-GOAL-AND-GENERATOR ('EYEHAND::GENERAL-SCAN) FOO).
;;; The peculiar arglist & quote are to allow some symbol to be used instead and
;;; have this change dynamically.  Note that we take the NAME of the goal, not
;;; the goal itself; we'll look it up.
(defmacro WITH-STARTING-GOAL-AND-GENERATOR ((goal-name &optional (mask '*goal-percept-set*))
					    &body body)
  `(with-microworld-action-selection-function (*goal-directed-learning-action-selector*)
     (with-masked-generators ,mask
       ;; Might as well leave the global value NIL, eh?  (Or is this a bad idea?  At
       ;; least this way I don't have to reset it in an UNWIND-PROTECT or anything,
       ;; but I could have the initializer for a run do that, anyway.  And what about
       ;; continuing a run from where it left off?  Should I instead have a macro
       ;; that just sets it up if it isn't, for use in continuations?)
       (let ((*goal* (find-goal ,goal-name)))
	 ;; So that switch-to-goal, when it inits goals, can fill the caches etc.
	 ;; [Actually, this isn't enough, because the rest of the caches aren't inited anyway.
	 ;; Looks like I have to call JUST-INIT once, even if we're going to do a full run here,
	 ;; just to get them inited!  What a losing design.]
	 (with-current-microworld ,goal-name
	   (switch-to-goal *goal*)		; Set up the percepts & actions for the first time.
	   ,@body)))))

;;; Convenience function.  Horrible name.
(defun STOP-WHEN-GOAL-ACHIEVED-ARB-FN (&key
				       (goal *goal*)
				       (stop-explanation
					 (format nil "Goal ~A achieved."
						 (goal-name goal))))
  (when (walk-goal-achieved? goal)
    stop-explanation))

;;;; "Combing" schemas.

;;; The idea here is to generate an artificial *SCHEMA-ARRAY* consisting only of
;;; schemas that would satisfy some particular goal.  The point is to load up a
;;; goal-independent run, then get rid of all the schemas that wouldn't have been
;;; generated in the first place if we had been running it on a goal-dependent fashion
;;; instead.  This isn't _quite_ the same as having run the goal-dependent run
;;; instead; in particular, we should be left with far fewer schemas than we would
;;; have if we're been doing that.  (I can also use this just to judge how many fewer
;;; schemas.)  This whole thing is entirely for evaluation, of course.

;;; &&& Um, this is exactly the wrong thing:  if percept-set is VF, this'll take anything
;;; &&& with VF in it.  What we _really_ want is anything that _doesn't_ mention anything
;;; &&& that _isn't_ VF!
;;; This binds its own copy *GOAL-PERCEPT-SET*.  You still shouldn't call this while
;;; goals are actively being run, though, because WALK-GOAL-SETS has to clear goal
;;; marks while it runs, and if some other process just happens to be walking them
;;; at the same time, they'll screw each other up.
(defun SCHEMAS-MENTIONING-GOAL (goal)
  (coerce-name->goal goal)
  (let ((*goal-percept-set* (make-maximal-bit-vector)))
    (walk-goal-percepts goal)
    (let ((iterator
	    (def-goal-directed-schema-percept-generator 'all-schema-numbers-generator))
	  (candidates nil))
      (with-iterator (yield-schema-index-fn reset-schema-index-fn iterator)
	(do-iterator (yield-schema-index-fn reset-schema-index-fn schema-index)
	  (push schema-index candidates)))
      candidates)))

(defun SCHEMAS-MENTIONING-ONLY-THESE-ITEMS (item-bv)
  (let ((inverse-item-bv (bit-not item-bv))	; These are all items which _aren't_ the ones we were handed.
	(mask (make-schema-bit-vector)))
    ;; Now compute all schemas which mention some item that we _weren't_ called with.
    (loop for item-index from 0 below *item-number*	; This loop mostly stolen from MAKE-GOAL-DIRECTED-SCHEMA-PERCEPT-GENERATOR.
	  do (unless (zerop (bit inverse-item-bv item-index))
	       (let* ((item (get-item item-index))
		      (c (item-context-dependent-schemas item))
		      (r (item-result-dependent-schemas item)))
		(bit-ior mask c t)	; T -> bash MASK.
		(bit-ior mask r t))))	; Ditto.
    ;; Finally, return a bitvector of those schemas which only mention the given items.
    (bit-not mask)))

;;; See caveats at SCHEMAS-MENTIONING-GOAL.
(defun SCHEMAS-MENTIONING-ONLY-THESE-ITEMS-IN-GOAL (goal)
  (coerce-name->goal goal)
  (let ((*goal-percept-set* (make-maximal-bit-vector)))
    (walk-goal-percepts goal)
    (schemas-mentioning-only-these-items *goal-percept-set*)))

;;; Returns a dummy *SCHEMA-ARRAY* of only those schemas which mention _only_
;;; the given items (e.g., if we're given item FOO, any schema which mentions any
;;; item that _isn't_ FOO is rejected).  Since items know which schemas depend
;;; upon them, we can't just return a new schema-array with all the schemas we
;;; want smashed down into the start of it.  Instead, we return one in which those
;;; positions which had schemas we don't want are replaced by a dummy schema
;;; (we use the same one in all such positions) which has no context, result, or
;;; action.  This means it can't participate in a plan, which is sufficient for our
;;; purposes.  (Obviously, the items which say that some dummy schema depends on
;;; them are now lying, but that's okay here.)
(defun FAKE-*SCHEMA-ARRAY*-OF-SCHEMAS-MENTIONING-ONLY-THESE-ITEMS-IN-GOAL (goal)
  (let ((mask (schemas-mentioning-only-these-items-in-goal goal))
	(dummy-schema (make-schema-internal -1 #'true))					    
	(new-*schema-array* (make-array *schema-maximum*)))
    ;; Fix up the dummy schema so SYN-ITEM-PHASE-2 doesn't blow out...
    ;; [It runs even if learning is disabled, since it has nothing to do with learning per se.]
    (setf (schema-parent dummy-schema) 0)	; Arbitrary.  Would otherwise be -1.
    ;; Keep SCHEMA-UPDATE-ALL-RESULTS from blowing up...
    (setf (schema-result-item dummy-schema) 0)	; Arbitrary.  Would otherwise be -1.
    ;; Now stuff the new array.
    (loop for i from 0 below *schema-number*
	  do (setf (aref new-*schema-array* i)
		   (if (zerop (bit mask i))
		       dummy-schema
		       (aref *schema-array* i))))
    new-*schema-array*))

;;; Precious!  Don't reset this if you've temporarily smashed *SCHEMA-ARRAY*.
;;; There's no reason for more than one element to be pushed on here, but
;;; we do it that way in case we somehow accidentally push a bad one on.
(defvar *SAVED-SCHEMA-ARRAYS* nil)

;;; Dangerous!  Tries to make sure *SCHEMA-ARRAY* is put back correctly
;;; after executing the body, but be careful anyway.  All the caveats about
;;; not running some other run while doing this go double here.
(defmacro WITH-COMBED-SCHEMA-ARRAY (goal &body body)
  `(unwind-protect
       (progn
	 (push *schema-array* *saved-schema-arrays*)
	 (setf *schema-array* (fake-*schema-array*-of-schemas-mentioning-only-these-items-in-goal ,goal))
	 ,@body)
     (setf *schema-array* (pop *saved-schema-arrays*))))

;;; End of file.
