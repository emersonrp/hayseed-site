;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Using the chaining logic to actually take actions, monitoring their results, that
;;;; lead to goals, and keeping track of the statistics of doing so (path lengths,
;;;; planning failures, etc).

(in-package :schema)

;;; The idea here is to build up "practice sequences", by having something that, e.g.,
;;; scans around until something's in the fovea, _then_ starts paying attention to the
;;; details.  Then, once it's achieved the end goal, it resets to the beginning and
;;; starts again (with some random delay to allow the world to get randomized on us
;;; again---otherwise, the object is still in the fovea, or whatever).
;;;
;;; The way we use all this is then to build up a bunch of schemas like this, _then_
;;; decide we've hit adolescence, stop learning, and go into pure performance mode.
;;; We do that by building chains, then putting the system back into the world with
;;; the learning system turned off (well, it doesn't have to be, but it's immaterial
;;; here, unless we can incrementally add to the chains, and it allows us to test the
;;; knowledge base very quickly by running experiments, and to run them several
;;; times with no learning happening in between).  Then we use the built chains to
;;; suggest the next action each time, using the GOAL-ACHIEVED-P part of each goal
;;; to know when it's time to start looking down the chains for a new goal.  We
;;; measure the time it takes to do this, and the number of times a chain fails (which
;;; means that either we give up, or we take random actions until we're back on a
;;; chain).
;;;
;;; This means that I map from some desired goal to be achieved to which parts of
;;; which chains need to be followed.  The thing to do is to find two sets:  set INITIAL
;;; contains all schemas whose contexts satisfy the current state of the world, and
;;; set FINAL contains all schemas for which, if their contexts were satisfied, the
;;; goal would be achieved.  Then, for each schema number in FINAL, we call
;;; CHECK-ITERATIVE-DEPTH-FIRST-FOR-SCHEMAS or similar functionality, and keep
;;; any chains that mention any schema in INITIAL.  We then pick some chain whose
;;; next schema from that initial point seems "best" (there are several possibilities;
;;; see the code) so far and take the indicated action, then replan.  If we ever get a
;;; planning failure, we record it and give up (or move randomly or something).  And
;;; we count how many actions we took in order to get to something in FINAL.  Then
;;; we do that whole thing several times to get something statistically significant,
;;; and then we pick some new goal or learn some more or something.
;;;
;;; How to generate this.  INITIAL is just those schemas whose APPLICABLE bit is on
;;; (set by SCHEMA-UPDATE-APPLICABLE).  FINAL is trickier, since goals only have a
;;; generalized function to determine when the goal is satisfied, rather than
;;; specifying explicitly which items must be on or off (indeed, goals such as
;;; WAIT-A-RANDOM-TIME cannot be so specified in the first place).  So using goals
;;; unmodified wouldn't work, because we're missing the information to compute
;;; which schemas should be in FINAL.  This means we have to add a slot to all goals,
;;; called SCHEMAS-FINAL, which holds code that selects schemas to consider as
;;; belonging in the set FINAL (e.g., if the code in the SCHEMAS slot, when called on a
;;; schema, returns T, the schema is considered to be in the set FINAL).

;;;; Conditions for goal failures, so we can trap them elsewhere and report.

;;; This is in support of repeatedly trying to compute paths and logging how many of
;;; them win as predicted, how many can't be planned, how many wind up being
;;; replanned but eventually winning, and how many of them win serendipitously.

;;; Not all oddities in goal-path planning are failures per se (I might wind up trapping
;;; serendipitous successes as conditions, or replans due to a changing world or
;;; something), so I don't want to say up front that they must all be failures.  Hence,
;;; I'm just calling 'em conditions.  Note, however, that they're all based on ERROR.
;;; This means that, if I fail to set up a handler to trap them, I expect to land in the
;;; debugger.  (Of course, in ANSI CL, as opposed to Genera, this _also_ requires that
;;; such conditions are signalled with ERROR or CERROR, _not_ SIGNAL!  ANSI CL
;;; SIGNAL is defined to just return NIL if nobody handles.)
(define-condition GOAL-PATH-CONDITION (error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Goal-path condition of type ~S." condition))))

(defun SHORT-CIRCUIT-REPORT (stream shorts pnames-too)
  (format stream "INITIAL/FINAL shorts between these schemas:  ")
  (cond (pnames-too
	 (loop for short in shorts
	       do (format stream "~&~2T~5D  ~A"
			  short
			  (schema-print-name (get-schema short)))))
	(t
	 (format-print-list stream "~D~&" shorts))))

(define-condition GOAL-PATH-SHORT-CIRCUIT (goal-path-condition)
  ((shorts
     :initarg :shorts
     :reader   goal-path-shorts))
  (:report (lambda (condition stream)
	     (short-circuit-report stream (goal-path-shorts condition) t)))
  )

(define-condition GOAL-PATH-NO-PATH (goal-path-condition)
  ()
  (:report (lambda (condition stream)
	     (format stream "Can't plan a path from any initial to any final.")))
  )

(define-condition GOAL-PATH-DANGLING-ENDS (goal-path-condition)
  ()
  )

(define-condition GOAL-PATH-EMPTY-INITIAL (goal-path-dangling-ends)
  ()
  (:report (lambda (condition stream)
	     condition				; Ignored.
	     (format stream "Goal-path initial schema set empty.")))
  )

(define-condition GOAL-PATH-EMPTY-FINAL (goal-path-dangling-ends)
  ()
  (:report (lambda (condition stream)
	     condition				; Ignored.
	     (format stream "Goal-path final schema set empty.")))
  )

(def-debugging-switch GOAL-PATH-CONDITIONS-SIGNAL t)

;;;; Showing status.

;;; These have to be defined here, because a logical macro of theirs is needed early.
;;; Note that we use GOAL-EXECUTION-SHOW-CURRENT-PLAN to override any setting
;;; of *SHOW-ITEMS-ENABLED* (from the SHOW-ITEMS debugging switch).  This means
;;; that, if GOAL-EXECUTION-STATUS-USE-LOG is T, it'll wind up in the log anyway, even if
;;; *SHOW-ITEMS-ENABLED* is nil.  This is to make it easy to show the state of the
;;; world when executing goals separately from the learning phase.
(def-debugging-switch GOAL-EXECUTION-SHOW-WORLD-STATE t)
(def-debugging-switch GOAL-EXECUTION-SHOW-CURRENT-PLAN t)
(def-debugging-switch GOAL-EXECUTION-STATUS-USE-LOG nil)

(defmacro WITH-GOAL-EXECUTION-STATUS-STREAM (&body body)
  `(let ((*output-stream*
	   (if *goal-execution-status-use-log-enabled*
	       *output-stream*
	       t)))
     ,@body))

;;;; Utilities for determining which schemas are in FINAL or INITIAL.

;;; This is specialized for the simple cases below; there's no point to overgeneralizing it.
(defmacro-definer DEF-RUNTIME-INITIALIZED-ARRAY (array-name array-length
					 initializer-name)
  `(progn
     (defvar ,array-name nil)
     (defsubst ,initializer-name ()
       (unless (and ,array-name
		    (= (length ,array-name) ,array-length))
	 (setf ,array-name (make-array ,array-length)))
       (fill ,array-name nil)
       ,array-name)))

(def-runtime-initialized-array *GOAL-SCHEMA-FINAL-ARRAY*
			       *schema-maximum*
  CREATE-GOAL-FINAL-ARRAY)

;;; Goal must already be initialized, of course.
(defun SCHEMA-INDICES-SELECTED-AS-GOAL-FINAL (goal)
  (create-goal-final-array)
  (let ((filter (goal-schemas-final goal)))
    (unless filter
      (error "Goal ~S doesn't have any final schemas defined."
	     (goal-name goal)))
    (with-schema-context-items-as-microworld-state
      (loop for index from 0 below *schema-number*
	    do (with-current-schema-as-context-items-for-microworld-state (get-schema index)
		 (when (funcall filter)
		   (setf (aref *goal-schema-final-array* index) t))))))
  *goal-schema-final-array*)

(def-runtime-initialized-array *GOAL-SCHEMA-INITIAL-ARRAY*
			       *schema-maximum*
  CREATE-GOAL-INITIAL-ARRAY)

;;; This is just the schemas which are applicable right now.
;;; The current goal doesn't matter, but we'll leave the name, for parallel construction.
(defun SCHEMA-INDICES-SELECTED-AS-GOAL-INITIAL ()
  (create-goal-initial-array)
  (loop for index from 0 below *schema-number*
	when (schema-applicable-p (get-schema index))
	  do (setf (aref *goal-schema-initial-array* index) t))
  *goal-schema-initial-array*)

;;; +++ Error-checking of the above.

;;; Makes sure that there's something in a set.
(defsubst ARRAY-NONEMPTY? (array)
  (some #'identity array))

;;; If this is supposed to signal, it won't bother to also print something.
(defsubst GRIPE-IF-ARRAY-EMPTY (array gripe condition)
  (unless (array-nonempty? array)
    (if *goal-path-conditions-signal-enabled*
	(schema-cerror "Continue anyway." condition)
	(with-goal-execution-status-stream
	  (format *output-stream* "~&~A~&" gripe)))
    t))

(defsubst ENSURE-INITIAL-AND-FINAL-NONEMPTY ()
  (or (gripe-if-array-empty *goal-schema-initial-array* "No initial schemas selected." 'goal-path-empty-initial)
      (gripe-if-array-empty *goal-schema-final-array*   "No final schemas selected."   'goal-path-empty-final)))

;;; Discovers whether any schemas in INITIAL are also in FINAL.  This is a "short
;;; circuit" in that it means that we don't have to do anything to satisfy the goal,
;;; and is clearly a degenerate case that shouldn't be used in performance
;;; evaluation.
(defun INITIAL-FINAL-SHORT-CIRCUIT? (&optional verbose pnames-too)
  (let ((shorts
	  (loop for i from (min-relevant-schema) below (max-relevant-schema) ; A short involving a schema that's in a lobotomized region doesn't count.
		when (and (aref *goal-schema-initial-array* i)
			  (aref *goal-schema-final-array* i))
		  collect i)))
    (when shorts
      (when verbose
	(with-goal-execution-status-stream
	  (format *output-stream* "~&")		; Actual error-reporter can't use newlines 'cause also used for condition's :REPORT method.
	  (short-circuit-report *output-stream* shorts pnames-too)
	  (format *output-stream* "~&")))
      (when *goal-path-conditions-signal-enabled*
	(schema-cerror "Continue anyway." 'goal-path-short-circuit
		       :shorts shorts))
      t)))

;;;; Finding paths from INITIAL to FINAL.

;;; We shouldn't try to compute the entire cross product from every initial schema
;;; to every final schema, because not all of them will ever be in the relevant set.
;;; (And, of course, because a 1K-schema run means that the cross product is a
;;; million!  Yet we only need to sample that space very sparsely, perhaps 1-3%, in
;;; practice.)  On the other hand, since a large percentage of all schemas are in
;;; either set at any given time (especially in INITIAL, which could have a third of all
;;; schemas in it at times), we shouldn't throw away the work.  Since we're not
;;; running in a context in which further learning could be taking place, we don't
;;; have to worry about a previously-found path being invalidated by a shorter one
;;; (between the same two schemas) appearing later, so we can cache every result
;;; that we find, to speed up plan execution.
;;;
;;; The cache is a simple hash table.  Each key must contain both the initial and final
;;; schema number.  While we _could_ use a cons to hold them, we'd then have to
;;; use EQUAL to search the table, and we'd cons every time we had to build a key to
;;; do the lookup.  Instead, we treat the initial & final schema numbers as two axes
;;; of an array and map the "array" into a one-dimensional array (by normal 2D
;;; array indexing rules), then make the lookup key based on that.  (Actually, we also
;;; round up the major axis to the next multiple of 10, so visual inspection can break
;;; the key apart, but that's just for debugging.)
;;;
;;; An additional complication, of course, is that the cache must be cleared at the
;;; same time that the other reachability caches are cleared.  To ensure that all of
;;; that happens at the same time, we build it into the other cache-clearing logic.

(defun COMPUTE-INITIAL-FINAL-HASH-KEY-MULTIPLIER (n)
  (expt 10 (ceiling (log n 10))))

(deflimit *INITIAL-FINAL-HASH-KEY-MULTIPLIER*
	  (compute-initial-final-hash-key-multiplier *schema-maximum*)
	  (compute-initial-final-hash-key-multiplier *schema-maximum*))

(defsubst INITIAL-FINAL-HASH-KEY (initial final)
  (+ (* initial *initial-final-hash-key-multiplier*)
     final))

;;; Find all chains leading from any schema number in INITIAL to any one in FINAL.
;;; $OPT:
;;; &&& I wonder if this should attempt to release unpromising paths early on?
;;; &&& It makes the modularity difficult (in that it would have to have the metric
;;; &&& that FIND-BEST-PATH-IN-INITIALS-TO-FINALS needs), but otherwise it can cons
;;; &&& lists with 10-20K elements, only to toss all but one of them anyway!  I think
;;; &&& the two functions should be combined to avoid running out of space in the middle
;;; &&& from the excessive consing (I can run out during this before GC could release the
;;; &&& list after its return).
;;;
;;; NOTE that the LAST schema in the path shouldn't have its action taken:  it's only
;;; returned so that, when its CONTEXT is satisfied, we know that the goal is achieved.
;;; So given a returned path of (A B C D), you should only actually execute (A B C).
(defun FIND-ALL-INITIALS-FROM-FINALS ()
  (let ((results nil))
    (dotimes-noting-progress (final (max-relevant-schema) "Chaining initials to finals")
      ;; This tends to cons a lot of intermediate trash; don't let it kill the machine unnecessarily.
      ;; It takes 60ms just to check, and doing that (say) 3600 times would take 22 seconds!  Yet
      ;; checking only outside the outer loop could let the machine cons itself to death.  So we check
      ;; every hundred times around the outer loop, which seems quite reasonable.
      (when (zerop (mod final 100))			 
	(wait-for-more-paging-if-necessary))
      (when (aref *goal-schema-final-array* final)
	(loop for initial from 0 below (max-relevant-schema)
	      when (and (aref *goal-schema-initial-array* initial)
			(schema-reachable? initial final))
		do (let* ((key (initial-final-hash-key initial final))
			  (result-path
			    (multiple-value-bind (lookup-path found?)
				(gethash key *cached-initials-to-finals*)
			      (cond (found?
				     lookup-path)
				    (t
				     (let ((path (iterative-depth-first-generate-action-sequence
						   initial final
						   :assume-reachability t)))	; We already checked its reachability just above.
				       ;; Even if the path is NIL, we should record this fact, so we don't have to rederive it.
				       (setf (gethash key *cached-initials-to-finals*) path)
				       path))))))
		     (when result-path		; Only report it if there's a path there.
		       (push result-path results))))))
    results))

;;; METRIC is some function that, when applied to a path, computes its merit.  Higher
;;; merits are better; a merit is inverse cost.  The path is a list of schema numbers.
;;; Typical metrics might be inverse length (which is biased towards shorter paths)
;;; or something that multiplies reliabilities (so that higher is better).  If we find two
;;; or more paths that are equally good, it is undefined which one is returned.  It
;;; returns both the best path and its merit.
;;;
;;; See comment at FIND-ALL-INITIALS-FROM-FINALS.
(defun FIND-BEST-PATH-IN-INITIALS-TO-FINALS (metric &optional precomputed-paths)
  (let ((paths (or precomputed-paths
		   (find-all-initials-from-finals)))
	(best-path nil)
	(best-merit most-negative-double-float))
    (when (and (null paths)
	       *goal-path-conditions-signal-enabled*)
      (schema-cerror "Continue anyway." 'goal-path-no-path))
    ;; Some metrics might underflow (since they're computing merits, and some
    ;; might be vanishingly small, in particular, too small to represent as a
    ;; normalized single-precision float).  However, asking each individual metric to
    ;; trap this would be both annoying and possibly expensive (setting up the trap
    ;; handler has some overhead to it).  So just do it once, outside the loop.  Any
    ;; metric that _really_ wants to see these (I can't imagine why, but some
    ;; perverse debugging regime might require it) can always surround itself with
    ;; a WITH-FLOATING-UNDERFLOW-TRAPS.
    (without-floating-underflow-traps
      (noting-progress ("Evaluating path metrics")
	(loop with total = (length paths)
	      for counter from 0
	      for path in paths
	      for merit = (funcall metric path)
	      do (note-progress counter total)
	      when (> merit best-merit)
		do (setf best-path  path)
		   (setf best-merit merit))))
    (values best-path best-merit)))

;;;; Path metrics:  General.

;;; Any metric defined here should take a path, which is a list of schema numbers,
;;; and compute a merit.  Higher merits are better.

;;; For metrics which look at reliability, we're therefore multiplying reliability
;;; together at each step, since we're computing merit, and higher merits are better.
;;; Note that computing reliabilities will also tend to select for short paths, since
;;; more reliabilities, even for reliable schemas, are a fair bit less than one, and it
;;; takes a pretty damned reliable set of schemas to compensate for having an extra
;;; one on the path.

;;; See also the comment at FIND-ALL-INITIALS-FROM-FINALS; these metrics shouldn't
;;; include the final schema in any given path.

;;; Note that, since we're figuring out which metric to call based on the setting of a
;;; global variable, making the metric itself a subst won't speed anything up (since it
;;; can't be open-coded, because we don't know until runtime which one it's gonna
;;; be).  On architectures with significant function-calling overhead, this might be a
;;; problem.  On Genera, though, at least, computing the best of even a 30K-path set
;;; takes only a second or two anyway (for PATH-RELIABILITY-METRIC-ALL, at least),
;;; so perhaps this doesn't matter too much, at least for Genera.
;;;
;;; Of course, it doesn't hurt to make them substs anyway, in case something else
;;; might be calling them in an inner loop...

;;; Succeeding pages have the various metrics.  The actual, toplevel metric,
;;; something that you could bind *CHAIN-TO-GOALS-METRIC* to, is called out by
;;; putting ->->->METRIC immediately before it.  (Perhaps I should track these with
;;; a special form, so I can easily find out what my choices are?)

;;;; Path metric:  simple multiplied reliability.

(defsubst SCHEMA-INDEX-RELIABILITY (index)
  (schema-reliability (get-schema index)))

;;; Runs the given metric over the path.
;;;
;;; NOTE that we are assuming that the path has NOT been reversed for us; in other
;;; words, it's what the iterative-deepening code might return.  This means that the
;;; FIRST schema index in the path is really the LAST on in the true path that we'd
;;; actually execute, which means we should NOT include it in evaluating the path's
;;; merit (because we'd never execute the action associated with that schema; c.f.
;;; comments at FIND-ALL-INITIALS-FROM-FINALS).
;;;
;;; REDUCE-FN might be, e.g., #'+ if KEY-FN was #'(lambda (ignore) 1), but I suppose
;;; it'd be easier to just replace the whole damned call with (1- (LENGTH PATH)) in
;;; that case.  So assume that by default we'll multiply, and maybe there's some
;;; case in which we'd want to add or something, but I can't think of one right now.
(defmacro REDUCING-PATH (path metric-fn &optional (reduce-fn #'*))
  `(reduce ,reduce-fn ,path
	   :key ,metric-fn			; Genera doesn't open-code either this or #'(lambda () ...).  Feh.
	   :start 1))				; Don't evaluate first (meaning really last) schema's metric.

;;; ->->->METRIC
;;; The simplest one:  just multiply the reliabilities together.
(defun PATH-RELIABILITY-METRIC (path)
  (reducing-path path #'schema-index-reliability))

;;;; Path metric:  reliability times enumeration of predicted bits.

;;; A more complicated case; this one favors schemas that are both reliable and
;;; which specify more sense bits.  Hence, an ideal path element would be a schema
;;; with high reliability that specifies lots of different sense bits for its context and
;;; result, meaning it makes a reliable prediction about a lot of world state.
;;;
;;; For efficiency, we store the number of sensory bits that each schema refers as
;;; a scalar, so we don't have to scan through two state-arrays every time we
;;; reencounter some schema.  For even more efficiency, we'd store this
;;; information directly in each schema, once and for all, when the schema is
;;; generated, but that would require adding a couple of slots to each schema, and
;;; I'm loath to do that, given the DEFSTRUCT-based (instead of object-based)
;;; implementation of schemas at present, and the existence of the snapshotter
;;; (which must deal with changes like this), so we store it in a couple of ancillary
;;; arrays, instead, and keep track of whether the arrays need updating.  (It's this
;;; check which makes this less efficient than direct storage.)  For flexibility, we
;;; store the number of context bits and the number of result bits separately, which
;;; means we have to do an add each time; too bad.
;;;
;;; NOTE that this one is buggy!  Because it allows the individual merit of some
;;; schema, which is (* reliability cardinality) for this metric, to exceed unity, and
;;; because merits are computed by multiplying these individual values together, any
;;; schema whose individual merit _does_ exceed unity will cause its associated path
;;; to be favored---which means that we favor uselessly long paths which have lots
;;; of these hangers-on in them.  In short, any individual merit metric that allows
;;; values greater than unity has reversed the sign of the pressure toward shorter
;;; paths, and will tend to favor longer and longer paths.  This is clearly the wrong
;;; idea.  So see the _next_ metric for a fix.

;;; +++ Actually computing how many context or result bits are on in some schema.
(defsubst SCHEMA-CONTEXT-CARDINALITY (schema)
  (cond ((not (zerop (schema-context-empty schema)))
	 0)
	((not (zerop (schema-context-single schema)))
	 1)
	(t
	 ;; The hard case.  Note that this isn't necessarily a conj (it might not be
	 ;; reliable enough yet), so we can't just look up the conj and count things in it.
	 ;; On the other hand, the context is always in the context-array, so we can count in there.
	 (state-array-cardinality (schema-context-array schema)
				  *fixna-required-to-hold-all-item-states*))))	 

(defsubst SCHEMA-RESULT-CARDINALITY (schema)
  (cond ((not (zerop (schema-result-empty schema)))
	 0)
	((zerop (schema-result-conj schema))
	 1)
	(t
	 ;; Must be a conj here.
	 (state-array-cardinality (conj-item-array (get-conj (schema-result-item schema)))
				  *fixna-required-to-hold-all-item-states*))))
;;; ---

;;; +++ Some interesting debugging functions.  Not commented out, because I may
;;; +++ want to use them for analysis.
(defun TSCC ()
  (loop for i from 0 below *schema-number*
	collect (schema-context-cardinality (get-schema i))))

(defun TSRC ()
  (loop for i from 0 below *schema-number*
	collect (schema-result-cardinality (get-schema i))))

(defun AVERAGE (list)
  (float (/ (reduce #'+ list)
	    (length list))))

(defun SHOW-AVERAGE-SCHEMA-BITS ()
  (let ((c-bits (tscc))
	(r-bits (tsrc)))
    (let ((c-ave (average c-bits))
	  (r-ave (average r-bits))
	  (c-max (maximize c-bits))
	  (r-max (maximize r-bits)))
      (format t "~&~D schema~:P.~&~
                   ~D average context bits per schema (~D max).~&~
                   ~D average result  bits per schema (~D max).~&~
                   ~D average bits per schema.~&"
	      (1- *schema-number*)
	      c-ave c-max
	      r-ave r-max
	      (+ c-ave r-ave))))
  (values))
;;; ---

;;; +++ *sigh*  All this hair is because I don't wanna add a couple slots.  Whatta loss... 
(def-runtime-initialized-array *GOAL-SCHEMA-CONTEXT-BITS-ENUMERATION*
			       *schema-maximum*
  CREATE-GOAL-SCHEMA-CONTEXT-BITS-ENUMERATION)

(def-runtime-initialized-array *GOAL-SCHEMA-RESULT-BITS-ENUMERATION*
			       *schema-maximum*
  CREATE-GOAL-SCHEMA-RESULT-BITS-ENUMERATION)

(defvar *GOAL-SCHEMA-ENUMERATION-LIMIT* -1)	; This is the number of the first schema NOT stored, e.g., should be equal to *SCHEMA-NUMBER*.

(def-microworld-independent-init RESET-GOAL-SCHEMA-ENUMERATION-LIMIT ()
  (setf *goal-schema-enumeration-limit* -1))	; Make sure it's reset if the number of schemas drops!

;;; Obviously, this doesn't interlock with anything, so don't run it while we're
;;; generating schemas in another process.
;;;
;;; [M3, 1220 schemas, 1.13 seconds first time, 80 uS subsequent times.
;;;  Times 30K paths, each averaging 3 schemas, and that's 7.2 seconds,
;;;  not including initialization.  Of course, the last schema's
;;;  reliabiliy doesn't matter, so maybe this is an average path
;;;  length of only 2 schemas, thus only 4.8 seconds.  This is the extra
;;;  time that doing this will cost us when running the goal metric;
;;;  perhaps we should somehow arrange to run it only once anyway?]
(defsubst MAYBE-INIT-GOAL-SCHEMA-BITS-ENUMERATION ()
  (unless (and *goal-schema-context-bits-enumeration*
	       *goal-schema-result-bits-enumeration*
	       (= (length *goal-schema-context-bits-enumeration*) *schema-maximum*))	; Assume that the result one will follow suit...
    (create-goal-schema-context-bits-enumeration)
    (create-goal-schema-result-bits-enumeration)
    (reset-goal-schema-enumeration-limit))
  (unless (= *goal-schema-enumeration-limit* *schema-number*)
    (loop for i from 0 below *schema-number*
	  do (setf (aref *goal-schema-context-bits-enumeration* i) (schema-context-cardinality (get-schema i)))
	     (setf (aref *goal-schema-result-bits-enumeration* i)  (schema-result-cardinality  (get-schema i))))
    (setf *goal-schema-enumeration-limit* *schema-number*))
  (values))

(defsubst SCHEMA-INDEX-CACHED-CONTEXT-CARDINALITY (index)
  (aref *goal-schema-context-bits-enumeration* index))

(defsubst SCHEMA-INDEX-CACHED-RESULT-CARDINALITY (index)
  (aref *goal-schema-result-bits-enumeration* index))
;;; ---

(defsubst SCHEMA-INDEX-RELIABILITY-TIMES-BITS (index)
  (* (schema-reliability (get-schema index))
     (+ (schema-index-cached-context-cardinality index)
	(schema-index-cached-result-cardinality index))))

;;; ->->->METRIC
;;; You can only call this one if you've somehow arranged for
;;; MAYBE-INIT-GOAL-SCHEMA-BITS-ENUMERATION to be called by, e.g.,
;;; FIND-BEST-PATH-IN-INITIALS-FROM-FINALS before it starts calling the metric
;;; function in its inner loop.  So far, there's no mechanism for doing that.
(defsubst PATH-RELIABILITY-TIMES-BITS-METRIC (path)
  (reducing-path path #'schema-index-reliability-times-bits))

;;; ->->->METRIC
;;; [Strictly speaking, this one, too, is a victim of not adding the slots...
;;; But it has to come second, because it's a subst, so the first is open-coded...]
(defsubst PATH-RELIABILITY-TIMES-BITS-MAYBE-UNINITIALIZED-METRIC (path)
  (maybe-init-goal-schema-bits-enumeration)
  (path-reliability-times-bits-metric path))

;;; +++
;;; Strictly debugging.  Remember that the paths from
;;; FIND-ALL-INITIALS-FROM-FINALS are all reversed.
(defun PATHS-MENTIONING-SCHEMA-INDEX (index &optional (cached-paths (find-all-initials-from-finals)))
  (remove-if-not #'(lambda (path)
		     (find index path))
		 cached-paths))

(defun PATHS-MENTIONING-SCHEMA-NAME (name &optional (cached-paths (find-all-initials-from-finals)))
  (let ((index (schema-name->index name)))
    (assert (not (null index)))
    (paths-mentioning-schema-index index cached-paths)))

;;; It's ridiculous that I have to go through this hair!  But schemas don't actually
;;; record which single item is in their context, if their context is single.  Feh.
(defun SCHEMA-CONTEXT-ONLY-THIS-ITEM? (schema item item-positive?)
  (catch 'right?
    (let ((context (schema-context-array schema))
	  (desired-state (if item-positive? (make-state-on) (make-state-off)))
	  (count 0))
      (dotimes (x (length context))
	(dotimes (y *states-in-fixnum*)
	  (let ((seen-state (state-array-get-state-specific context x y)))
	    (cond ((= count item)
		   (unless (state-eq desired-state seen-state)
		     (throw 'right? nil)))	; In other words, wrong.
		  (t
		   (unless (state-unknown-p seen-state)
		     (throw 'right? nil))))	; In other words, wrong.
	    (incf count)))))
    t))						; Right.

;;; This is simplistic; it doesn't take into account the difference between
;;; (A&B)/foo/bar and A&B/foo/bar, for example, and probably has other bugs.
(defun SCHEMA-CLAIMS-NO-EFFECT? (schema)
  (let ((c-conj (schema-context-conj schema))
	(r-conj (schema-result-conj schema)))
    (and (flag-eq c-conj r-conj)		; Either they're both conj's, or they're both not.
	 (or (and (flag-truep c-conj)		; If they're conj's, ...
		  (= (schema-context-item schema) (schema-result-item schema)))	; ... the conj's must be the same.
	     (and (flag-falsep c-conj)		; If they're not conj's...
		  (not (zerop (schema-context-single schema)))  ; ... then the single item in the schema...
		  (schema-context-only-this-item?    ; ... must match the result item ...
		    schema
		    (schema-result-item schema)	; ... in number ...
		    (flag-falsep (schema-result-negated schema))))))))	; ... and sense (on or off).

(defun SCHEMA-NUMBERS-CLAIMING-NO-EFFECT ()
  (loop for i from 0 below *schema-number*
	when (schema-claims-no-effect? (get-schema i))
	  collect i))

(defun SCHEMAS-CLAIMING-NO-EFFECT ()
  (mapcar #'get-schema-fn (schema-numbers-claiming-no-effect)))
;;; ---

;;;; Path metric:  reliability times _normalized_ enumeration of predicted bits.

;;; This is like the previous path metric, but with its unfortunate tendency to favor
;;; long paths corrected.  We do this by making sure that each individual merit is
;;; below unity, by simply dividing the cardinality of each schema by the total
;;; number of items, yielding a "normalized" cardinality.  (Logically, we do this by
;;; dividing each of the context & result cardinalities by the number of items, so that
;;; even a schema which specifies all items in both context & result can't have an
;;; normalized cardinality above one.  Actually, since we're summing those two
;;; cardinalities anyway, we divide the sum by twice the number of items, simply to
;;; make the math slightly faster.  Ideally, we'd normalize like this when storing the
;;; result into the enumeration arrays, and that might be a reasonable thing to do, iff
;;; I decide that this is really the _right_ solution _and_ the performance is even
;;; noticeable (I doubt it would be, except on some machine that doesn't have
;;; hardware division---and how many of those could there _be_ these days?)
;;;
;;; Note that this doesn't work well, either.  By normalizing in this fashion, we exert
;;; a _very_ strong selective pressure for short paths---because no schema has
;;; more than a few percent of all items specified, every schema added to the path
;;; beats down the total merit by something like a couple orders of magnitude.
;;; Given that, no reliability, no matter how good, can hope to beat the negative
;;; effects of lengthening the path by even one schema, so we tend to go for the
;;; shortest possible path (e.g., one action, i.e., two schemas, of which we don't
;;; count the last one).  Among all possible one-action paths, we then pick the one
;;; with the biggest individual merit, which might favor a relatively unreliable
;;; schema that just happens to have a bunch of items and hence should have been
;;; pretty meritless.  This was seen in the first run, wherein we picked
;;; "/eyel/(vf20&vf21)" (a contextless schema, for god's sake!), which, not
;;; unsurprisingly considering its lack of context, has a reliability of only
;;; 3.4745854e-14:  ridiculously small, but probably the best individual merit of the
;;; applicable schemas (e.g., those whose result mentioned some item we need to be
;;; on for the goal to be satisfied)---of which most probably only had _one_ item in
;;; their result, so they lost.  *sigh*
;;;
;;; See the next metric for a better idea.

(defsubst SCHEMA-INDEX-RELIABILITY-TIMES-NORMALIZED-BITS (index)
  (* (schema-reliability (get-schema index))
     (/ (+ (schema-index-cached-context-cardinality index)
	   (schema-index-cached-result-cardinality index))
	(* *item-number* 2))))			; Any decent compiler should turn this into (ASH *ITEM-NUMBER* 1), of course, given fixna declarations...

(defsubst PATH-RELIABILITY-TIMES-NORMALIZED-BITS-METRIC (path)
  (reducing-path path #'schema-index-reliability-times-normalized-bits))

;;; ->->->METRIC
;;; [Strictly speaking, this one, too, is a victim of not adding the slots...
;;; But it has to come second, because it's a subst, so the first is open-coded...]
(defsubst PATH-RELIABILITY-TIMES-NORMALIZED-BITS-MU-METRIC (path)
  (maybe-init-goal-schema-bits-enumeration)
  (path-reliability-times-normalized-bits-metric path))

;;;; Path metric:  reliability _to the power of_ enumeration of predicted bits.

;;; This should work better.  It tends to nail relatively reliable schemas that mention
;;; several items up near, but certainly never above, unity, while very quickly
;;; beating relatively unreliable schemas into the dust (and the most items they
;;; mention, the worse they'll fare).  Perhaps the curve shouldn't beat the unreliable
;;; ones down quite so fast, but I'm not sure how to do that.
;;;
;;; NOTE that this is NOT the _normalized_ cardinality!  That would basically yield the
;;; same answer as the above metric (and _did_, when I accidentally wrote it that
;;; way at first), because we'd be exponentiating to tiny powers (like 1e-14 or
;;; whatever), rather than to small positive integer powers instead (like 2 or 4).

(defsubst SCHEMA-INDEX-RELIABILITY-EXPT-BITS (index)
  (expt (schema-reliability (get-schema index))
	(+ (schema-index-cached-context-cardinality index)
	   (schema-index-cached-result-cardinality index))))

(defsubst PATH-RELIABILITY-EXPT-BITS-METRIC (path)
  (reducing-path path #'schema-index-reliability-expt-bits))

;;; ->->->METRIC
;;; [Strictly speaking, this one, too, is a victim of not adding the slots...
;;; But it has to come second, because it's a subst, so the first is open-coded...]
(defsubst PATH-RELIABILITY-EXPT-BITS-MU-METRIC (path)
  (maybe-init-goal-schema-bits-enumeration)
  (path-reliability-expt-bits-metric path))

;;;; Getting the whole ball of wax rolling.

(def-reported-parameter *CHAIN-TO-GOALS-PREDICATE* 'accessible-test)

;;; +++
;;; If this is set, we'll call it to initialize common state before evaluating any
;;; metrics.  We'll call it upon entry to PLAN-BEST-PATH-TO-GOAL, so it should still
;;; probably make use of caching, etc, but this way the metric itself (which might be
;;; called around 100K times during the creation of a single plan) doesn't have to
;;; check each time.  [In the case of MAYBE-INIT-GOAL-SCHEMA-BITS-ENUMERATION,
;;; which is efficiently coded but nonetheless takes 80 microseconds on a MacIvory
;;; Model 3 to conclude that the caches are current, this saves us 7-8 seconds per
;;; plan.]
;;;
;;; Note that there's no particular reason to report the setting of this in a log
;;; (unless I suspect that I set -METRIC* without this...), so it's just DEFPARAMETER.
(defparameter *CHAIN-TO-GOALS-METRIC-INITIALIZER* 'maybe-init-goal-schema-bits-enumeration)

;;; This is the actual metric to use.  If it requires initiailzation before it'll work, it
;;; had better be in sync with *CHAIN-TO-GOALS-METRIC-INITIALIZER*!
; (def-reported-parameter *CHAIN-TO-GOALS-METRIC* 'path-reliability-metric)
(def-reported-parameter *CHAIN-TO-GOALS-METRIC*   'path-reliability-expt-bits-metric)

;;; Easy way to set the above, and to remember to set the initializer if necessary.
(defmacro WITH-CHAIN-TO-GOALS-METRIC ((metric &optional initializer) &body body)
  `(let ((*chain-to-goals-metric* ,metric)
	 (*chain-to-goals-metric-initializer* ,initializer))
     ,@body))
;;; ---

;;; +++
;;; Not foolproof, but probably a good idea.
;;; Might recompute them if somebody called ordinary SCHEMA-CHAINING-ROOT,
;;; which doesn't record the predicate used to compute the set.
(defvar *LAST-USED-SCHEMA-CHAINING-ROOT-PREDICATE* nil)	; Recompute on first call.

(defsubst MAYBE-RESET-SCHEMA-CHAINING-ROOT (&optional dont-notify)
  (unless (and (eq *last-used-schema-chaining-root-predicate* *chain-to-goals-predicate*)
	       (loop for i from 0 below *schema-number*	; SOME isn't too useful, 'cause it doesn't have an :END keyword...
		     thereis (schema-chain (get-schema i))))
    (unless dont-notify
      (format t "~&Generating schema chaining roots using ~A...~&"
	      *chain-to-goals-predicate*))
    (let ((*default-schema-chaining-predicate* *chain-to-goals-predicate*))
      (prog1					; If SCHEMA-CHAINING-ROOT ever returns multiple values, this'll have to be MULTIPLE-VALUE-PROG1.
	(schema-chaining-root)
	(setf *last-used-schema-chaining-root-predicate* *chain-to-goals-predicate*)	; Set this last, in case we bombed above.
	))))

(defun PLAN-BEST-PATH-TO-GOAL-INIT (goal-or-name)
  (maybe-reset-schema-chaining-root)
  (schema-indices-selected-as-goal-final (name->goal goal-or-name))
  (schema-indices-selected-as-goal-initial)
  (when *chain-to-goals-metric-initializer*
    (funcall *chain-to-goals-metric-initializer*)))

;;; See comment at FIND-ALL-INITIALS-FROM-FINALS.
(defun PLAN-BEST-PATH-TO-GOAL (goal-or-name &optional (sc-verbose t) (sc-pnames-too t))
  (plan-best-path-to-goal-init goal-or-name)
  (unless (or (ensure-initial-and-final-nonempty)    ; This will bitch if something's empty.
	      (initial-final-short-circuit? sc-verbose sc-pnames-too))	; This will bitch if there's a short circuit somewhere.
    (find-best-path-in-initials-to-finals *chain-to-goals-metric*)))
;;; ---

;;;; Using the above.

;;; Notice that this runs iterations of the schema system (with learning disabled!) on
;;; its own.  This is very unusual, in that usually these are being run by
;;; RUN-MICROWORLD.  So note that none of the usual logfile or bindings etc are in
;;; effect.  I suppose I could think about how to invert this so that this _becomes_
;;; the action-selection-function if we were to run in that context (by storing the
;;; current path, for use iff we're not replanning every step, and computing it the
;;; first and/or other times, then just returning it as the action).

(defvar *DESIRED-PLANNING-GOAL* nil)

;;; Sets up the context required to run PICK-BEST-NEXT-ACTION-TO-GOAL.
(defmacro WITH-CHAINING-TO-GOALS-ACTIONS (goal &body body)
  `(let ((*desired-planning-goal* (name->goal ,goal)))
     (with-learning-inhibited
       ,@body)))

;;; Broken out because this is useful to call at the very end of a run, too.
(defun GOAL-ORIENTED-MAYBE-SHOW-WORLD-STATE ()
  (when *goal-execution-show-world-state-enabled*
    (if *goal-execution-status-use-log-enabled*
	(eyehand::eyehand-show-items-enabled)
	(eyehand?))))

;;; This is only here so it can be set down in the guts of action selection, and
;;; investigated by monitoring tools elsewhere (for things like replans, plan lengths,
;;; etc).  I don't want to make this a meter, because I want to keep more compact
;;; statistics (and because it's really not something I want to preserve with a run).
(defvar *CURRENT-GOAL-PLAN* nil)

;;; +++
;;; The idea here is to allow COLLECT-ONE-SCORECARD to say, "Make a plan and run it to
;;; expected completion." The way this works is to cache the very first plan we make,
;;; then to use successive steps of it along the way, without replanning.
;;;
;;; The protocol works as follows.  If *ALLOW-INCREMENTAL-REPLANNING* is NIL, then
;;; we check to see if *UNREPLANNED-PLAN* is also NIL.  If it is, we plan a path, and
;;; set it to that path.  If it wasn't NIL, we just pop off the next action in the plan,
;;; and run that.
;;;
;;; NOTES:  Someone higher up (presumably, COLLECT-ONE-SCORECARD) is responsible
;;; for clearing *UNREPLANNED-PLAN* whenever they want us to replan.  We also
;;; depend on an ARB-STOP-FN that will stop when *CURRENT-GOAL-PLAN* contains
;;; only one action left (this is the last piece of the plan whose action should not be
;;; executed, and is there just so we can check its context).
;;;
;;; If *ALLOW-INCREMENTAL-REPLANNING* is non-NIL, we don't bother keeping
;;; *UNREPLANNED-PLAN* up to date.

(def-reported-parameter *ALLOW-INCREMENTAL-REPLANNING* nil)

(defvar *UNREPLANNED-PLAN* nil)

;;; Since this gets called right at the start, before we've even taken a first action,
;;; we can't simply ask "Is the plan short?", because the plan will be NIL (since we
;;; haven't taken an action, which is what causes planning), and NIL's pretty short.
;;; Instead, we ask, "Is the plan exactly 1 long?", which is the right thing.
;;;
;;; Um, now that we're running the plan one step at a time, even in
;;; nonincremental-planning mode, this actually causes us to screw up in
;;; COLLECT-ONE-SCORECARD (we think that every replan-at-end-of-path is also a
;;; stymied-no-effect, because we enter EXECUTE-BEST-PLAN-TO-GOAL, this is T (at
;;; the very end), we return without performing any action whatsoever, and of
;;; course it looks like we were stymied.  So I think that this is a bad idea in that
;;; regime.  Our caller (e.g., COLLECT-ONE-SCORECARD) should just do what it was
;;; doing in the incremental-planning case, which is to see if the path has shortened
;;; sufficiently.  (This is exactly what we're doing it, but COLLECT-ONE-SCORECARD
;;; will do it at the right time, whereas we won't.)
(defun NONINCREMENTAL-PLANNING-ARB-STOP-FN ()
  (= (length *current-goal-plan*) 1))

(defun PLAN-OR-CONTINUE-BEST-PATH-TO-GOAL (goal)
  (flet ((make-a-plan ()
	   ;; If we're about to try maknig a brand-new plan, zero *CURRENT-GOAL-PLAN*
	   ;; first.  This is entirely so that, if we take an error and throw, it'll be NIL and
	   ;; not the old, obsolete plan, which had nothing to do with the error we took.
	   ;; (This is so our caller can reason about the current plan better.)
	   (setf *current-goal-plan* nil)
	   ;; A note about the REVERSE here.  All of the path-planning logic produces the
	   ;; path in _backwards_ order (e.g., the goal is the first item, and the current
	   ;; state is the last item), because it produces the path using PUSH and (when
	   ;; backtracking) POP.  Rather than call REVERSE on every path so generated
	   ;; (which would double the amount of consing---and no, NREVERSE is no help,
	   ;; being even slower than REVERSE on a freshly-consed, CDR-coded list), we
	   ;; call REVERSE at the absolute last minute---before we're about to print the
	   ;; path, store it in permanent data structure, or execute it.  We do this because
	   ;; there are lots of other paths that we generated that weren't as good (as
	   ;; determined by the desirability metric), and we can prune those without
	   ;; reversing them.
	   (reverse (plan-best-path-to-goal goal))))
    (let ((plan
	    (cond (*allow-incremental-replanning*
		   (make-a-plan))
		  (t
		   (cond (*unreplanned-plan*
			  (cdr *current-goal-plan*))
			 (t
			  (setf *unreplanned-plan* (make-a-plan))
			  *unreplanned-plan*))))))
      (setf *current-goal-plan* plan)
      plan)))
;;; ---

;;; This is not the most efficient way to do it, because some of the computation
;;; (e.g., SCHEMA-INDICES-SELECTED-AS-GOAL-FINAL) might be precomputed.  I'll
;;; fix this later.
;;;
;;; This should be run inside WITH-CHAINING-TO-GOALS-ACTIONS.  Also, somewhere
;;; inside there, but outside here, somebody has to check to ensure that we stop
;;; asking to take actions once the goal is achieved.
;;;
;;; See comment at FIND-ALL-INITIALS-FROM-FINALS.
;;; &&& Um, I'm not sure that we won't try to take the very LAST schema in a
;;; &&& path as an action, if it turns out that the penultimate schema (which was
;;; &&& supposed to cause us to be in a state whereby the last schema's context
;;; &&& was satisfied) doesn't actually do so.  Of course, for this to be the case,
;;; &&& the current plan would have to still contain only the last schema, and
;;; &&& nothing else; should I worry about this?  By definition, the plan's failed
;;; &&& if we wind up in this state, after all...
;;;
;;; NOTE that PLAN-OR-CONTINUE-BEST-PATH-TO-GOAL and its callees might _signal_!
;;; This new snake in paradise means that RUN-ONE-ITERATION-1 (which is what is
;;; our eventual caller) may try to run an action, and then be thrown through,
;;; without being able to update its state.  However, since the signal will happen
;;; _before_ the action is taken, we won't have updated the world in any way,
;;; so this _should_ be safe, if my analysis is correct.
(defun PICK-BEST-NEXT-ACTION-TO-GOAL (&rest ignore)
  (let* ((path (plan-or-continue-best-path-to-goal *desired-planning-goal*))
	 (first-actionable-schema (get-schema (car path)))
	 (action-index (schema-action-item first-actionable-schema)))
    (setf *current-goal-plan* path)
    ;; Everything from here down is for debugging, and shows what we're doing.
    (goal-oriented-maybe-show-world-state)
    (when *goal-execution-show-current-plan-enabled*
      (with-goal-execution-status-stream
	(format *output-stream* "~&Current plan:~&")
	(loop for schema-number in path
	      do (format *output-stream* "~&~2T~5D  ~A~&"
			 schema-number
			 (schema-print-name (get-schema schema-number))))
	(format *output-stream* "~&Taking action ~D (~@:(~A~))~&"
		action-index
		(strip-action-name (get-action-print-name action-index)))))
    ;; Return the action we should execute next.
    action-index))

(defmacro WITH-GOAL-DIRECTED-PLANNED-ACTION-SELECTOR (&body body)
  `(with-microworld-action-selection-function ('pick-best-next-action-to-goal)
     ,@body))

(defun EXECUTE-BEST-PATH-TO-GOAL-CB-ARB-STOP (show-success)
  (maybe-initialize-all-goals)
  (let ((stop-reason
	  (stop-when-goal-achieved-arb-fn
	    :goal *desired-planning-goal*)))
    (when (and stop-reason show-success)
      (format t "~&~A~&" stop-reason))
    stop-reason))

;;; You could use BUNCH-SIZE as a limit, in case the goal is never achieved somehow.
;;; We don't screw around with BUNCH-SIZE internally here, nor do we clear *UNREPLANNED-PATH*
;;; here.  We leave those up to the caller, which presumably knows what it's doing and gets both
;;; of them right in either the incremental or nonincremental case.
;;;
;;; NOTA BENE that, when PICK-BEST-NEXT-ACTION-TO-GOAL (called from
;;; WITH-GOAL-DIRECTED-PLANNED-ACTION-SELECTOR) sets *CURRENT-GOAL-PLAN*,
;;; what we've really done is to note what the _whole_ plan looks like, but that we
;;; have then _taken the first action_ in that plan by the time we've returned from
;;; this routine right here.  (That's assuming that _our_ caller called us with a
;;; :BUNCH-SIZE of 1; if we got called with a larger BUNCH-SIZE, then we'll have
;;; executed that many more actions in the returned path.)
(defun EXECUTE-BEST-PATH-TO-GOAL (goal-or-name &rest checkpoint-bunches-keywords &key
				  show-success	; Print a message (not to the log) if we win.
				  (cb-arb-stop
				    #'(lambda ()	; Dont this way so it can lexically capture SHOW-SUCCESS.
					(execute-best-path-to-goal-cb-arb-stop show-success)))
				  &allow-other-keys)
  ;; Necessary anyway (*sigh*!), because we have to do planning work outside of
  ;; CHECKPOINT-BUNCHES, etc, to see if we're done yet.  Should maybe make the
  ;; action-selection-function set some variable instead?  Hmm.
  (with-goal-directed-planned-action-selector
    (with-chaining-to-goals-actions goal-or-name
      (apply #'checkpoint-bunches
	     :arbitrary-stop-fn cb-arb-stop					
	     :number-of-bunches 1		; Stop when we're done with this bunch!
	     :never-snapshot t			; Don't save state.
	     :start-new-run nil			; We can't be starting a new run (else we wouldn't know anything).
	     (rem-keywords checkpoint-bunches-keywords
			   '(:show-success)))))
  (goal-oriented-maybe-show-world-state)
  (values))

;;; Just an easier-to-call version of the above (an invoker, really).
(defun VERBOSE-EXECUTE-BEST-PATH-TO-GOAL (goal-or-name &rest checkpoint-bunches-keywords &key
					  (show-paths t)  ; Show the plan at each step.
					  (eyehand? t)	  ; Show what the world looks like at each step.
					  (show-success t)	; Print a message (not to the log) if we win.
					  use-log   ; Put all the drivel in the log instead of *TERMINAL-IO*.
					  (log-name-prefix "goals")
					  (signal t)
					  &allow-other-keys)
  (let ((*goal-execution-show-current-plan-enabled* show-paths)	; Make it easier to set these.
	(*goal-execution-show-world-state-enabled* eyehand?)
	(*goal-execution-status-use-log-enabled* use-log)
	(*goal-path-conditions-signal-enabled* signal))
    (apply #'execute-best-path-to-goal
	   goal-or-name
	   :show-success show-success
	   :log-name-prefix log-name-prefix
	   checkpoint-bunches-keywords)))

;;;; Performance evaluation:  trying several times to reach a goal, while keeping
;;;; statistics on how well we're doing.

;;; +++
;;; Important point:  The action might succeed EARLY if there was a quicker way
;;; there that we didn't know to use, but we happened to intersect it while pursuing
;;; a longer path!
;;; ---

;;; +++
;;; $OPT:  I should seriously consider adding WRITE-ROOTS-AND-CACHES to either the
;;; snapshot dumping code, or at least to somethere in this file, so I can easily
;;; preserve that information.  (It can take nearly as long to regenerate it as the run
;;; took in the first place!)
;;;
;;; I might also wanna see if the restoration code zaps the preserved caches, and, if
;;; so, have it do something else maybe.  Otherwise, I can't reset the state of the
;;; world for debugging or whatever without losing those caches (unless I dump &
;;; reload them to the filesystem, which is unacceptable.)
;;; ---

;;; There's one scorecard per type of goal that we've tried.
(defstruct (GOAL-SCORECARD (:CONC-NAME GS-))
  (n 0)					; Number of times we've tried with this goal.
  (goal nil)				; The goal we're trying to achieve (as a real goal, not a name).
  (wins 0)				; How many times we were able to plan _and_ reach the goal.  Does NOT include serendipity or aimless-wins!
  (stymied-initial 0)			; How many times we weren't even able to begin planning (INITIAL empty).
  (stymied-final 0)			; How many times we weren't even able to begin planning (FINAL empty).
  (stymied-cant-start 0)		; How many times we couldn't compute an initial plan.
  (stymied-cant-continue 0)		; How many times we were suddenly unable to continue after a replan (no new plan can be formed).
  (stymied-short-circuit 0)		; How many times we were suddenly short-circuited.  [&&& Isn't this equivalent to completed?]
  (stymied-no-effect 0)			; How many times no primitive items were changed by the action we just took.
  (serendipity 0)			; How many times we won earlier than the currently-amended plan thought we would.
  (done-at-start 0)			; How many times we were done before taking any action at all.
  (replans 0)				; How many times the current plan changed (ignoring simple shortening as it's executed).
  (replan-loops 0)			; How many times we exceeded *SCORECARD-REPLANS-LIMIT* when trying to collect a card.
  (aimless-steps 0)			; Number of aimless steps we took while hoping to encounter a state we can plan from.
  (totally-frustrated 0)		; Number of times we gave up after taking *SCORECARD-FRUSTRATION-TIME* aimless steps.
  (ave-winning-plan-length 0)		; Average length of any winning plan or replan (from when we initially planned it, not at the end, of course).
  (ave-serendipitous-plan-length 0)	; Average length of the remaining length of the plan when we discovered we were serendipitously done.
  (contextless-path-starts 0)		; Number of times we picked a schema with an empty context as the start of the best path.
  )

;;; The number of "real attempts" that this scorecard actually reflects.  That means
;;; that we actually thought we should try to plan towards the goal, regardless of
;;; whether we then aborted or got a serendipitous result or whatever.  In
;;; particular, this excludes done-at-starts and shorts, since those don't indicate
;;; anything about our planning prowess.
(defun SCORECARD-REAL-ATTEMPTS (card)
  (- (gs-n card)
     (gs-stymied-short-circuit card)		; Um, this lets out shorts that happen in the middle, too.  But they almost always happen at the start...
     (gs-done-at-start card)))

;;; Utility that I keep needing in various places.
;;; NOTE that the use of string is:
;;;   To result in a string, not a symbol coming back, and
;;;   To strip the package prefix off the name.
;;; If you need the package prefix to be there, then you'll
;;; just have do this by hand.
(defsubst CARD->GOAL-NAME (card)
  (string (goal-name (gs-goal card))))

;;; This must stay in sync with FORMAT-SCORECARD.  Note that this assumes that no
;;; goal is shorter than four characters; if so the columns will be misaligned.
(defun FORMAT-SCORECARD-HEADER (&key (goal-width 1)	; For better tabular formatting, if we're being clever.
				(show-goal t)
				;; If you supply either of the prefix strings, you must supply padding for the other, at least.
				(prefix-string-1 "")	; These will need a trailing space or more.
				(prefix-string-2 "")
				(postfix-string-1 "")	; These will need a leading space or more.
				(postfix-string-2 "")
				(stream t))
  (italic-format
    stream "~&~A~:[~2*~;~VA ~]       --Expected- ------Unexpected-------  ----------Stymied------------===== Despr Relaxation  World~A~&"
    prefix-string-1 show-goal goal-width "    " postfix-string-1)
  (italic-format
    stream "~&~A~:[~2*~;~VA ~]    N  Wins  WPLen Seren SPLen Repln RplnL  Init Final Strt Cont Efct ShCkt Ctxt Aimls Frust DoneS~A~&"
	  prefix-string-2 show-goal goal-width "Goal" postfix-string-2)
  (values))

;;; This must stay in sync with FORMAT-SCORECARD-HEADER.
(defun FORMAT-SCORECARD (card &key (goal-width 1)	; For better tabular formatting, if we're being clever.
			 (show-goal t)
			 (prefix-string "")	; This will need a trailing space or more.
			 (postfix-string "")	; This will need a leading space or more.
			 (stream t)
			 (newlines t))
  (format stream "~:[~;~&~]~A~:[~2*~;~VA ~]~5D ~5D  ~5,2F ~5D ~5,2F ~5D ~5D ~5D ~5D ~5D ~5D ~5D ~5D ~5D ~5D ~5D ~5D~A~:[~;~&~]"
	  newlines
	  prefix-string
	  show-goal goal-width (card->goal-name card)
	  (gs-n card)
	  (gs-wins card)
	  (gs-ave-winning-plan-length card)
	  (gs-serendipity card)
	  (gs-ave-serendipitous-plan-length card)
	  (gs-replans card)
	  (gs-replan-loops card)
	  (gs-stymied-initial card)
	  (gs-stymied-final card)
	  (gs-stymied-cant-start card)
	  (gs-stymied-cant-continue card)
	  (gs-stymied-no-effect card)
	  (gs-stymied-short-circuit card)
	  (scl:ignore-errors			; &&& The IGNORE-ERRORS should go away as soon as I don't have any pre-Schema-12.89 cards left.
	    (gs-contextless-path-starts card))
	  (gs-aimless-steps card)
	  (gs-totally-frustrated card)
	  (gs-done-at-start card)
	  postfix-string
	  newlines)
  (values))

(defun FORMAT-SCORECARDS (cards &key (goal-width 1 gw-supplied-p)	; For better tabular formatting, if we're being clever.
			  (show-goals t)
			  (stream t)
			  (header t)
			  ;; These are only used when formatting an individual card.
			  (prefix-format-string "~A")	; I could avoid calling format with a supplied-p arg, but that seems excessive.
			  (prefix-format-args '(""))
			  (postfix-format-string "~A")
			  (postfix-format-args '(""))
			  ;; These are only used for the heading, if requested.
			  (prefix-string-1 "")	; These will need a trailing space or more.
			  (prefix-string-2 "")
			  (postfix-string-1 "")	; These will need a leading space or more.
			  (postfix-string-2 ""))
  (let ((use-goal-width
	  (when show-goals
	    (if gw-supplied-p
		goal-width
		(longest-printed-rep
		  cards #'card->goal-name)))))
    (when header
      (format-scorecard-header
	:goal-width use-goal-width
	:show-goal show-goals
	:prefix-string-1  prefix-string-1
	:prefix-string-2  prefix-string-2
	:postfix-string-1 postfix-string-1
	:postfix-string-2 postfix-string-2
	:stream stream))
    (dolist (card cards)
      (format-scorecard
	card
	:goal-width use-goal-width
	:show-goal show-goals
	:prefix-string  (apply #'format nil prefix-format-string  prefix-format-args)
	:postfix-string (apply #'format nil postfix-format-string postfix-format-args)
	:stream stream)))
  (values))

(defvar *SCORECARD* nil)			; Current scorecard (so we don't have to hand this all over the place).
(defvar *ALL-SCORECARDS* nil)			; All the scorecards we've tried so far, to be keyed by goal.  (When should this be cleared?)

(defparameter *SCORECARD-GOALS-TO-TRY*		; The goals we should be trying.  We'll try them in rotation.
;  	      '(eyehand::general-scan-until-in-fovea-wif))
	      '(eyehand::high-res-scan-into-dead-center))

(defparameter *SCORECARD-RELAXATION-TIME* 20)	; How many clock ticks to dither in between trying one goal and the next, after successful completion.
(defparameter *SCORECARD-FRUSTRATION-TIME* 20)	; How many clock ticks to try random actions before declaring this goal unsatisfiable and picking the next.
(defparameter *SCORECARD-REPLANS-LIMIT* 20)	; How many replans we can do in a single card before deciding we must be looping (e.g., chess stalemate).

;;; If this is NIL, we don't write into the normal *OUTPUT-STREAM* log while running scorecards.
;;; Since we're not learning, such writing is pretty uninformative, and quickly eats a large amount
;;; of disk space, since we're constantly starting runs & writing that large preamble every time.
(def-debugging-switch SCORECARD-LOGGING nil)

(defun GET-SCORECARD (goal &optional create-p)
  (coerce-name->goal goal)
  (let ((found (find goal *all-scorecards* :key #'gs-goal)))
    (cond (found
	   found)
	  (t
	   (unless create-p
	     (error "Couldn't find scorecard for goal ~S in ~S."
		    goal
		    *all-scorecards*))
	   (let ((new (make-goal-scorecard :goal goal)))
	     (push new *all-scorecards*)
	     new)))))

(defun CREATE-NEEDED-SCORECARDS ()
  (loop for goal in *scorecard-goals-to-try*
	do (get-scorecard goal t))
  (values))

(defun SCORECARD-GOALS-TO-TRY-CIRCULARLY ()
  (apply #'circular-list *scorecard-goals-to-try*))

(defun SCORECARD-TAKE-AIMLESS-STEPS (n log-name-prefix &key
				     (runner-fn *default-runner-fn*)
				     (snapshot-fn *default-snapshot-fn*)
				     show-iterations)
  (with-learning-inhibited			; Don't learn during this random step.
    (let ((*show-iterations* show-iterations))
      (checkpoint-bunches
	:bunch-size n
	:number-of-bunches 1			; Stop when we're done with this bunch!
	:never-snapshot t			; Don't save state.
	:start-new-run nil			; We can't be starting a new run (else we wouldn't know anything).
	:log-enabled *scorecard-logging-enabled*
	:log-name-prefix log-name-prefix
	:runner-fn runner-fn
	:snapshot-fn snapshot-fn
	))))

;;; Note that this goes to *STANDARD-OUTPUT*, not the log.
;;; That's why it's called REPORT- and not LOG-
(def-debugging-switch REPORT-SCORECARD-INCFS t)

(defmacro SC-REPORTED-INCF ((accessor &rest args))	; Extra level of parens so this is called like INCF.
  `(progn
     (when *report-scorecard-incfs-enabled*
       (format t "~&INCF of ~A~&" ',accessor))
     (incf (,accessor ,@args))))

;;; This presupposes that the update of N (the number of old samples over which the
;;; old average was computed) has NOT yet been updated!  (Yeah, in the limit, this
;;; doesn't matter, but we're talking about rather small values of N here...)
(defsubst UPDATE-AVERAGE-WITH-NEW-SAMPLE (old-n old-average new-value)
  (/ (+ (* old-n old-average)
	new-value)
     (1+ old-n)))

(defsubst UPDATE-AVERAGE-PLAN-LENGTH (old-n old-average plan)
  (update-average-with-new-sample
    old-n old-average
    (1- (length plan))))

;;; Takes the length of PLAN (minus one, because the last schema in the plan isn't
;;; supposed to be executed), and updates the average plan length for this card.
;;; You can call this for any average plan length sort of field maintained by the card.
(defmacro UPDATE-CARD-PLAN-AVERAGE (card ave-accessor n-accessor plan)
  `(setf (,ave-accessor ,card)
	 (update-average-plan-length
	   (,n-accessor card)
	   (,ave-accessor ,card)
	   ,plan)))

(defun UPDATE-CARD-FOR-WINNING-PLAN (card plan)
  (update-card-plan-average
    card
    gs-ave-winning-plan-length
    gs-wins
    plan))

(defun UPDATE-CARD-FOR-SERENDIPITOUS-PLAN (card plan)
  (update-card-plan-average
    card
    gs-ave-serendipitous-plan-length
    gs-serendipity
    plan))

;;; This is internal to COLLECT-ONE-SCORECARD.  It's just defined here to
;;; save consing & disposing of the array (yet another resource).
(defvar *COS-PREV* (make-state-array *microworld-state-size*))

;;; Compares only the primitive (e.g., nonsynthetic) items in the two state arrays.
;;; Why?  Because otherwise we never really know when an action we just took
;;; didn't "really" have an effect, because the syn-items take a while to "decay" or
;;; are otherwise updated asynchronously.  This is obviously a kluge.  So sue me.
;;; [Note:  This simple implementation only takes 3ms.  Doesn't seem worth writing a
;;; clever one that compares word-by-word until the last word.]
(defun STATE-ARRAY-PRIMITIVE-ITEMS-ONLY-EQUAL (one two)
  (loop for i from 0 below (primitive-item-number)
	always (state-eq (state-array-get-state one i)
			 (state-array-get-state two i))))

(defun COLLECT-ONE-SCORECARD (card &key log-name-prefix
			      (runner-fn *default-runner-fn*)
			      (snapshot-fn *default-snapshot-fn*)
			      show-iterations)	; Whether to count out iterations while relaxing.
  (wait-for-more-paging-if-necessary)		; Paranoia; let's not lose a long run unnecessarily.
  (setf *scorecard* card)			; &&& Do I really need to bother with this?  We'll see...
  (setf *current-goal-plan* nil)		; Zero out any old plan from some previous run.
  (setf *unreplanned-plan* nil)			; Force fresh planning.
  (let ((*show-iterations* show-iterations)
	;; Note that PRESERVED-GOAL-PLAN is supposed to hold the latest complete plan (including any replanning we
	;; may have had to do) that was supposed to lead to a win.  Once we make the plan (or forcibly replan, e.g.,
	;; the new plan is not the old plan minus the just-executed step), we hang onto this, and don't shorten it as
	;; we shorten the plan-we're-currently-executing at each step.  If we win, we record how long this plan was.
	(preserved-goal-plan nil)
	(aimless-step-counter -1))		; Set & reset inside the loop.  -1 means we're not being aimless at the moment.
    (flet ((being-aimless? ()
	     (not (minusp aimless-step-counter))))
      (loop with previous-path and stymied-no-effect?
	    with replans-this-time = 0
	    for count from 0
	    ;; Compute all of these flags up front, THEN decide whether to terminate.  That way, I can reliably use their results in the
	    ;; FINALLY clause.  (Otherwise, some of them will have been updated, and some will still have the value from the last iteration,
	    ;; and it's totally dependent upon exactly which clause bailed out which are updated and which are old.)
	    for done? = (with-current-microworld
			  (goal-name (gs-goal card))	; Goal must be in same package as microworld by definition.  Yuck, I hate this.
			  (walk-goal-achieved? (gs-goal card)))
	    for frustrated? = (> aimless-step-counter *scorecard-frustration-time*)
	    for too-many-replans? = (>= replans-this-time (1- *scorecard-replans-limit*))
	    ;; *CURRENT-GOAL-PLAN*, once set, has already had the first action in it taken.  So if we've planned at least once (e.g., COUNT is at least 1
	    ;; and this is not the very first iteration of this loop), make sure that *C-G-P* shows enough actions that we can do something.  That means
	    ;; at least three:  the first one has already been taken, and the last one should not be taken at all (only its context matters).  So if it's only
	    ;; two long at this point, then we've already theoretically finished the plan, and should have decided above we're done.  Since we didn't,
	    ;; the plan must have failed.
	    for plan-not-exhausted? = (or (null *current-goal-plan*)	; Either we haven't planned yet...
					  (> (length *current-goal-plan*) 2))	; ... or, if we have, there must be actions left to take.
; 	    do (format t "~&COLLECT-ONE-SCORECARD count ~D, current path ~S, previous path = ~S,~&~
;                             done? = ~S, frust? = ~S, tmr = ~S, pne = ~S, sne = ~S~&"	; DBG.
; 		       count *current-goal-plan* previous-path done? frustrated? too-many-replans? plan-not-exhausted? stymied-no-effect?)
; 	    do (format t "~&~S = ~S~&"		; DBG.
; 		       'previous-state (listarray previous-state))
; 	    do (format t "~&~S = ~S~&"		; DBG.
; 		       'current-state (listarray current-state))
	    until (or done? frustrated? too-many-replans? stymied-no-effect?)
	    while plan-not-exhausted?
	    do
	(cond ((being-aimless?)
	       (scorecard-take-aimless-steps 1 log-name-prefix)
	       (sc-reported-incf (gs-aimless-steps card))
	       (incf aimless-step-counter))
	      (t
	       (setf aimless-step-counter 0)	; If we don't reset this, we took an error return or made no progress and should be aimless for a while.
	       (setf previous-path *current-goal-plan*)	; Record what the plan was before we generate a new one.
; 	       (format t "~&Before EBPTG, ~S = ~S, ~S = ~S~&"	; DBG.
; 		       '*current-goal-plan* *current-goal-plan*
; 		       '*unreplanned-plan* *unreplanned-plan*)
	       (state-array-copy *microworld-state* *cos-prev* *microworld-state-size*)	; Preserve current microworld state in *COS-PREV*.
	       (condition-case (err)
		    (execute-best-path-to-goal	; This is what actually sets *CURRENT-GOAL-PLAN*.
		      (gs-goal card)
		      :bunch-size 1		; Always execute plan one step at a time, so we can check for serendipity, and ...
		      :log-enabled *scorecard-logging-enabled*	; ... so the plan-length averages are set.
		      :log-name-prefix log-name-prefix
		      :runner-fn runner-fn
		      :snapshot-fn snapshot-fn)
		  (goal-path-short-circuit
		    (sc-reported-incf (gs-stymied-short-circuit card)))
		  (goal-path-empty-initial
		    (sc-reported-incf (gs-stymied-initial card)))
		  (goal-path-empty-final
		    (sc-reported-incf (gs-stymied-final card)))
		  (goal-path-no-path
		    (if (zerop count)
			(sc-reported-incf (gs-stymied-cant-start card))
			(sc-reported-incf (gs-stymied-cant-continue card))))
		  (:no-error
		    (setf aimless-step-counter -1))	; Successfully executed this step of the plan.
		  )
; 	       (format t "~&After EBPTG, ~S = ~S, ~S = ~S~&"	; DBG.
; 		       '*current-goal-plan* *current-goal-plan*
; 		       '*unreplanned-plan* *unreplanned-plan*)
	       (cond ((being-aimless?)		; The things in the T clause only make sense to check if we think we successfully planned & ran an action.
		      (setf *unreplanned-plan* nil)) ; If we screwed up, force fresh planning next time.
		     (t
		      ;; Note that, if a plan has a schema with no context in it, it must be the first one in the plan, since otherwise it can't be chained to.
		      ;; Depends on PLAN-OR-CONTINUE-BEST-PATH-TO-GOAL zeroing *CURRENT-GOAL-PLAN* before it makes a new plan.
		      (when (and *current-goal-plan*	; If we couldn't get started on this plan, it doesn't make sense to check this.
				 (schema-context-empty-p (get-schema (car *current-goal-plan*)))
			(sc-reported-incf (gs-contextless-path-starts card)))
		      ;; See if our action changed the world.
		      (when (state-array-primitive-items-only-equal *microworld-state* *cos-prev*)
			(sc-reported-incf (gs-stymied-no-effect card))	; EYEL when already fully left, etc.
			(setf stymied-no-effect? t)))))	; LOOP-FINISH doesn't seem to be defined in this version of LOOP...
	       ;; These things make sense to check regardless of the outcome of the action ('cause they talk about planning, not the action itself).
	       (when (and *current-goal-plan*	; If we've figured out a plan, and we haven't preserved it yet, preserve it. ...
			  (not preserved-goal-plan))	; ... This captures the first plan for this call, but doesn't let it get shortened later.
		 (setf preserved-goal-plan *current-goal-plan*))
	       (when (and *current-goal-plan* previous-path
			  (not (equalp *current-goal-plan* (cdr previous-path))))
; 		 (format t "~&Replan:  previous = ~S, current = ~S~&"	; DBG.
; 			 previous-path *current-goal-plan*)
		 (setf preserved-goal-plan *current-goal-plan*)	; This is a replan, so update our expected winning plan.
		 (sc-reported-incf (gs-replans card))
		 (incf replans-this-time)
		 )))
	    finally
	      (unless (being-aimless?)		; If we got stuck or shorted out or something, don't bother checking this stuff.
		(when done?
		  ;; We actually finished successfully.  Figure out if it was because of something we can take credit
		  ;; for, or if it was serendipitous (either we stumbled onto it prematurely while running a plan,
		  ;; or we were taking aimless steps because we didn't know what to do, and hit it by accident).
		  ;; The latter isn't really counted as serendipity, because we were really running a plan then.
; 		  (format t "~&Done with previous = ~S, current = ~S~&"	; DBG.
; 			  previous-path *current-goal-plan*)
		  (cond ((zerop count)
			 (sc-reported-incf (gs-done-at-start card)))
			(t
			 (unless (being-aimless?)
			   (cond (plan-not-exhausted?
				  (update-card-for-serendipitous-plan
				    card *current-goal-plan*)	; Record the length of remainder after serendipity struck.
				  (sc-reported-incf (gs-serendipity card)))	; If we were serendipitous, don't also update the win counter.
				 (t
				  (update-card-for-winning-plan card preserved-goal-plan)	; Record the length of the winning plan.
				  (sc-reported-incf (gs-wins card)))))))))
	      ;; [Check this stuff even if we did take an error.]
	      ;; We may or may not have finished successfully; see if something bad
	      ;; happened that caused us to exit, and update counters for the reasons if so.
	      (when frustrated?
		(sc-reported-incf (gs-totally-frustrated card)))
	      (when too-many-replans?
		(sc-reported-incf (gs-replan-loops card)))
	      )))
  (sc-reported-incf (gs-n card)))		; Now that we're all done, we can increment this to let the world know.

;;; If you specify both STARTS and ATTEMPTS, we'll go until the first one is satisfied.
;;; In the one-kind-of-goal-this-run case, since ATTEMPTS cannot exceed STARTS, this
;;; means we'll collect n starts on one card & stop.  If there are multiple goals, we'll
;;; keep going either until we've run that many starts, or until each card (e.g., each
;;; goal) has accumulated the right number of attempts.
(defun COLLECT-SCORECARDS (&key new-cards	; Zap the initial set of scorecards before executing.
			   (attempts 0)		; How many "real" attempts, ignoring done-at-start etc.
			   (starts most-positive-fixnum)	; How many starting goals.
			   (log-name-prefix "scorecard")
			   show-iterations)	; Whether to count out iterations while relaxing.
  ;; All goals might not be inited, and we require them to be before EXECUTE-BEST-PATH-TO-GOAL
  ;; might do it for us, because COLLECT-ONE-SCORECARD must be able to call WALK-GOAL-ACHIEVED?
  ;; before it calls EXECUTE-BEST-PATH-TO-GOAL.  (Note the yucko fact that I have to enclose this thing
  ;; inside a WITH-CURRENT-MICROWORLD, because if any inits have to happen, various things like
  ;; ALL-FINE-FOVEA-ITEM-NUMBERS must run, and they need to know what microworld to use.)
  ;; This assumes that all goals in *SCORECARD-GOALS-TO-TRY* will be in the same package, which
  ;; seems like a reasonable assumption.
  (with-current-microworld (car *scorecard-goals-to-try*)
    (maybe-initialize-goal-sets))
  ;; Create the scorecards to hold the results.
  (when new-cards
    (setf *all-scorecards* nil))		; Zap old results, if desired.
  (create-needed-scorecards)			; Regardless of zappage, make sure we have the right new ones.
  ;; Now actually generate the results.
  (flet ((card-satisfied? (card)
	   (>= (scorecard-real-attempts card) attempts)))
    (let ((*checkpoint-bunches-await-gc-enabled* nil)	; Don't bother waiting for the GC, since we're doing tons of tiny, learning-inhibited runs.
	  (all-cards (mapcar #'get-scorecard *scorecard-goals-to-try*))
	  (count 0))
      (loop while (< count starts)
	    until (every #'card-satisfied? all-cards)
	    for goal in (scorecard-goals-to-try-circularly)
	    for card = (get-scorecard goal)
	    unless (card-satisfied? card)	; If this particular card is satisfied, spin around the loop and select another one.
	      do (incf count)			; This counts as a start.
		 (collect-one-scorecard
		   card
		   :show-iterations show-iterations
		   :log-name-prefix log-name-prefix)
		 (format t "~&Relaxing...  ")	; DBG.  [This only looks reasonable if SHOW-ITERATIONS is NIL.
		 (scorecard-take-aimless-steps
		   *scorecard-relaxation-time*
		   log-name-prefix
		   :show-iterations show-iterations)
		 (format t "Done.~&")	; DBG.
		 )))
  (values))

;;; A convenience function.
(defun SUMMARIZE-SCORECARDS (&rest keywords)
  (apply #'format-scorecards *all-scorecards* keywords))

;;;; Performance evaluation with lobotomies.

;;; A list of pairs.  Each pair consists of (LIMIT CARDS) information, where
;;; LIMIT is the lobotomy limit, and CARDS is a list of scorecards for that limit.
;;; The name -LP stands for "limit pairs".
(defvar *ALL-LOBOTOMIZED-SCORECARD-LPS* nil)

(defun ADD-LOBOTOMIZED-SCORECARDS (limit)
  (push (list limit *all-scorecards*)
	*all-lobotomized-scorecard-lps*)
  (setf *all-scorecards* nil))

;;; NOTE that this does _not_ reset *ALL-LOBOTOMIZED-SCORECARD-LPS*.
;;; The caller should decide when to flush this data, not us.
(defun COLLECT-SCORECARDS-WITH-LOBOTOMIES (&key new-cards	; Zap the initial set of scorecards before executing.
					   (attempts 0)	; How many "real" attempts, ignoring done-at-start etc.
					   (starts most-positive-fixnum)	; How many starting goals.
					   (log-name-prefix "scorecard")
					   ;; These are for determining the step boundaries & numbers.
					   (steps *number-of-knowledge-steps*)
					   (include-all t)
					   (low 0)
					   (high *schema-number*)
					   show-iterations)	; Whether to count out iterations while relaxing.
  (let ((all-steps (schema-numbers-from-steps
		     :steps steps
		     :include-all include-all
		     :low low
		     :high high)))
    (with-relevancy-cache-invalidated
      (loop for limit in all-steps
	    for counter from 1
	    do (bold-format t "~2&~\\datime\\ Collecting scorecards with lobotomy from ~D to ~D (step ~D of ~D)...~2&"
			    low limit counter (length all-steps))
	       (let ((*schema-relevancy-limit-high* limit))
 		 (reset-cache-relevancy t)
		 (setf *all-scorecards* nil)
		 (collect-scorecards
		   :show-iterations show-iterations
		   :starts starts
		   :attempts attempts
		   :new-cards new-cards
		   :log-name-prefix log-name-prefix)
		 (add-lobotomized-scorecards limit))
	       #+Genera (wait-for-gc-complete))))	; Let the GC catch up before the next iteration.
  (values))

;;;; Analyzing scorecards.

;;; Returns all the unique values of GOAL in the various cards.
(defun ALL-SCORECARD-TYPES (cards)
  (remove-duplicates
    (mapcar #'gs-goal cards)))

;;; This actually returns a list of scorecards.
(defun ALL-LOBOTOMIZED-SCORECARD-TYPES (l-sc-list)	; A list of lobotomized-scorecard info.
  (all-scorecard-types
    (loop for (limit cards) in l-sc-list
	  do (ignore limit)			; Ignored.
	  append cards)))

;;; This actually returns a list of scorecards.
;;; The returned list looks like an L-SC-LIST, but all cards in the CARDS
;;; piece of it that are not of type TYPE are removed.  TYPE is a GOAL
;;; (not the name of a goal).
(defun LOBOTOMIZED-LPS-OF-TYPE (l-sc-list type)
  (remove nil					; If we've removed all the cards from some pair, remove the pair as well.
	  (loop for (limit cards) in l-sc-list
		collect (list
			  limit 
			  (remove type cards
				  :test-not #'eql
				  :key #'gs-goal)))
	  :key #'second))

(defun ANALYZE-LOBOTOMIZED-SCORECARDS (&optional (lps *all-lobotomized-scorecard-lps*)
				       (stream t))
  (let ((types (all-lobotomized-scorecard-types lps)))
    (loop for count from 0
	  for type in types
	  for lps-of-type = (lobotomized-lps-of-type lps type)
	  do (unless (zerop count)
	       (terpri stream))
	     (bold-format stream "~&Goal ~A:~2&" (goal-name type))
	     (format-scorecard-header
	       :prefix-string-1 "      "	; Padding.
	       :prefix-string-2 "Limit "
	       :show-goal nil)
	     (loop for (limit cards) in lps-of-type
		   do (assert (= (length cards) 1))	; For a particular limit and type of card, there should only be one of them, right? ...
		      (format-scorecard		; ... (Assumes that LOBOTOMIZED-LPS-OF-TYPE correctly removes (x NIL) pairs...)
			(car cards)		; ... though it's a singleton list, so we still have to grab the contents.
			:prefix-string (format nil "~5D " limit)
			:show-goal nil))))
  (values))

;;;; Saving scorecard state, since it can take a while to generate it.

;;; These are uniformly tiny, so dump them to LMFS, where we can use long names.
(defparameter *SCORECARD-DUMP-PATHNAME* "schema:runs;scorecard-dump.ibin")
(defparameter *LOBOTOMY-DUMP-PATHNAME*  "schema:runs;lobotomy-dump.ibin")

;;; This is the valid data if we AREN'T also doing lobotomized runs.
(defun DUMP-SCORECARD-INFO (&optional (where *scorecard-dump-pathname*))
  (dump-variables
    where
    *all-scorecards*))

;;; This is the valid data if we ARE also doing lobotomized runs.
(defun DUMP-LOBOTOMY-INFO (&optional (where *lobotomy-dump-pathname*))
  (dump-variables
    where
    *all-lobotomized-scorecard-lps*))

;;; Utility.  Dumb name.
(defun QUIZ ()
  (analyze-lobotomized-scorecards)
  (terpri)
  (summarize-scorecards))

;;; End of file.
