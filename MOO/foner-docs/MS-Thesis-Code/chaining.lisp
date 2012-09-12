;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Support for chaining towards goals, in the absence of goal-directed actions.

;;; This includes building a digraph of all schema connections, searching the digraph
;;; in various ways, and generating a list of actions to be used in running the
;;; animation.

(in-package :schema)

;;;; Building a digraph of which schemas "imply" which others.

;;; For the discussion below, the term "relevant schema" implies a schema that fits at
;;; least the criteria of being in the right numerical range (used to simulate earlier
;;; stages of knowledge, when required, because schemas are added in order of
;;; invention), and being of at least the requisite reliability (presumably at least
;;; medium, and maybe high).  Additional requirements may be added in some
;;; particular run.

;;; The general idea is to function like a depth-first, mark-sweep GC.  We build a
;;; root set of all relevant schemas with null contexts and non-null results.  (Such
;;; schemas must be at the start of any chain, since, with null contexts, they cannot
;;; be chained to.)  For each root object, we traverse the set of all relevant,
;;; non-null-context schemas.  If we find a relevant, unmarked schema whose
;;; context chains from the result of the root object, we add it to the chain being
;;; built, mark the schema we just found, and then use the new schema as the base
;;; of a chain to be recursively extended.  At any point during this process, we
;;; abandon the attempt to extend the chain if we either cannot find a next schema
;;; to chain to, or if the next schema is already marked (to prevent loops)---a
;;; marked schema must, by definition, have already had its chain traversed, or to be
;;; in the process of having its chain traversed at the moment.  Assuming that there
;;; are no duplicates in the root set (a big assumption at the moment!), we cannot
;;; find that the first schema we find to extend the chain is already marked, since
;;; the only way that two contextless schemas in the root set have the same next
;;; schema in the chain is if their results are identical, and we just assumed that this
;;; cannot be.  Therefore, every schema in the root set that has any next schema
;;; will eventually wind up pointing at such a next schema.

;;; Currently, this is done inefficiently, in that we scan over all schemas each time
;;; we try to lengthen the chain.  We could use the item-dependencies being
;;; maintained by each item, though we're still have to prune those by whether each
;;; item was positively or negatively included.  If this turns out to be intolerably
;;; slow or something (or if it starts getting called on-demand, instead of just once),
;;; then this might be a reasonable optimization to include.  We're also inefficient in
;;; that we don't precompute the set of relevant schemas (either those with or
;;; without contexts), which would mean O(n) fewer calls to the relevancy
;;; predicate.
;;;
;;; Another source of inefficiency is the repeated use of GET-SCHEMA (e.g., AREF)
;;; and the passing around of indices instead of actual schemas.  We do this so that
;;; the relevancy predicate can nonetheless get its hands on indices, not schemas,
;;; for simulating less knowledge than is currently known (e.g., by only "noticing"
;;; schemas below a certain limit, hence only those schemas that were learned before
;;; a particular point in a run).

;;; For the moment, we use the spare slots in the schemas to hold the state info.  If
;;; I wind up doing this a lot, I suppose I can add slots.  Note that we could use the
;;; "marked" bit in the schema's data word to hold at least the marking status, but,
;;; since we've got two spare slots, we'll use them to make reading and writing
;;; marks faster, and to avoid possibly stepping on the toes of the schema algorithm
;;; if it's already using those in some way I haven't noticed.

;;; Note that, just because schemas are unique (in a bug-free world, anyway), this
;;; doesn't mean that a result schema can only point to one other schema.  After all,
;;; many schemas might have the same context, but specify different actions, hence
;;; leading to different results.

(defmacro SCHEMA-MARKED? (schema)
  `(schema-spare-slot-1 ,schema))

(defsubst MARK-SCHEMA (schema)
  (setf (schema-marked? schema) t))

(defsubst UNMARK-SCHEMA (schema)
  (setf (schema-marked? schema) nil))

(defmacro SCHEMA-CHAIN (schema)
  `(schema-spare-slot-2 ,schema))

;;; Watch out!  Some animation things might modify these without treating them as
;;; limits (e.g., without propagating anything that might depend on them).  If this
;;; becomes important, I'll modify such code to be more careful, but not now.
(deflimit *SCHEMA-RELEVANCY-LIMIT-LOW* 0 0)
(deflimit *SCHEMA-RELEVANCY-LIMIT-HIGH* *schema-maximum* *schema-maximum*)

;;; %%% OOPS.  This appears to be getting used only to generate the root set.
;;; %%% It looks like we find _all_ schemas after that, reliable or unreliable,
;;; %%% for chains.  I might as well just generate the entire root set (e.g.,
;;; %%% all schemas with empty contexts and nonempty results), and then
;;; %%% just prune while following a chain based on some criterion.
;;; %%% [Just made that true, by eliminating the call to this in
;;; %%% SCHEMA-CHAINING-ROOT-SET.]
;;;
;;; %%% Um, the proper place to be calling this, I think, is _not_ while generating
;;; the root set, and _not_ while determining which schemas chain to which others,
;;; but instead while _following_ a chain to determine reachability.  When following
;;; a chain, any schema which fails the relevancy test is just omitted.  Obviously,
;;; if we've cached data about this sort of thing, we'll have to flush the cache when
;;; the relevancy criteria change, but we _don't_ have to regenerate all the chains.
;;;
;;; For speed, and because I don't want to bother passing in all the args all over
;;; the place, this is just hardcoded.  Just change this function to try different
;;; relevancy criteria.
(defun SCHEMA-INDEX-RELEVANT? (index)
  (and (<= *schema-relevancy-limit-low*  index)
       (>  *schema-relevancy-limit-high* index)
;        (reliable-schema-index? index)
       ))

(defsubst MAX-RELEVANT-SCHEMA ()
  (min *schema-relevancy-limit-high* *schema-number*))

(defsubst MIN-RELEVANT-SCHEMA ()
  *schema-relevancy-limit-low*)

;;; Clear all the marks and all the chains.  The MARKS-ONLY arg is so we can use this
;;; for traversing all schemas, _after_ chaining (hence without clearing all the
;;; chaining data), without cycles.  Dangerous!  Can destroy the results of 20
;;; minutes of computation instantly.
(defun RESET-SCHEMA-CHAINING (&optional marks-only)
  (loop for i from 0 below *schema-number*
	for schema = (get-schema i)
	do (unmark-schema schema)
	   (unless marks-only
	     (setf (schema-chain schema) nil)))
  (values))

;;; To make it harder for me to accidentally omit the arg when I wanted it...
(defun RESET-SCHEMA-MARKS ()
  (reset-schema-chaining t))

;;; Returns indices, not schemas.
(defun SCHEMA-CHAINING-ROOT-SET ()
  (loop for i from 0 below *schema-number*
	for schema = (get-schema i)
	when (and (schema-context-empty-p schema)
		  (not (schema-result-empty-p schema)))
	  collect i))

;;; Figures out whether one schema chains to another.  Vaguely like ACCESSIBLE-TEST,
;;; but that function didn't work "correctly" (in my opinion) in the face of conjuctions.
;;; To wit, (ACCESSIBLE-TEST "-vp11/eyer/(-vf21&vf34)" "vf31/eyef/(-vp10&-vp11)")
;;; (e.g., is the former reachable from the latter) returns T, whereas I think it should
;;; return NIL.
;;;
;;; %%% I wonder if I should go back to ACCESSIBLE-TEST anyway.  After all, if we can
;;; reliably generate one piece of the conjunction, what's to prevent somebody from
;;; using the piece we generated?  (Of course, if various things along the way want
;;; the other pieces of the conjunction set differently, well...)
(defun SCHEMAS-CHAIN? (context-schema result-schema)
  (assert (and (not (schema-result-empty-p  result-schema))	; Paranoia.  (But paranoia is usually justified...)
	       (not (schema-context-empty-p context-schema))))
  (let ((result-data  (schema-data result-schema))
        (context-data (schema-data context-schema)))
    (cond ((schema-data-result-conj-p result-data)
	   ;; Result is a conj.  Unfortunately, the context might be a conjunction, or a set of items
	   ;; that haven't been turned into a conj yet, or a single item.
	   (cond ((schema-data-context-conj-p context-data)	; I wonder if both forks of this mean the same thing?
		  (equalp (get-conj (schema-result-item  result-schema)) ; Could probably just compare conj indices, not contents, ...
			  (get-conj (schema-context-item context-schema))))   ; ... except for duplicate conj's bug.
		 (t
		  (equalp (get-conj (schema-result-item  result-schema))
			  (schema-context-array context-schema)))))
	  (t
	   ;; Result is not a conj.  Make sure that the _only_ thing in the context is the result's item.
	   (when (schema-data-context-single-p context-data)	; Must be a single item, not a set of them, or a conj.
	     (state-eq (if (schema-data-result-negated-p result-data)
			   (make-state-off)
			   (make-state-on))
		       (state-array-get-state
			 (schema-context-array context-schema)
			 (schema-result-item result-schema))))))))

;;; This might be something like #'RELIABLE-SCHEMA? instead.
;;; It should take a schema as its single argument.  It'll only be called for
;;; schemas with nonempty contexts.  [Note that adding this filtering only
;;; increased the runtime of FIND-NEXT-CHAINED-SCHEMA by 10%.]
(defvar *DEFAULT-SCHEMA-CHAINING-FILTER* #'true)

(defvar *DEFAULT-SCHEMA-CHAINING-PREDICATE* 'schemas-chain?)	; Might also be ACCESSIBLE-TEST.

;;; Actually finds the next schema in the chain.
(defun FIND-NEXT-CHAINED-SCHEMA (start-from result-schema)	; E.g., not an index.
  (let ((filter *default-schema-chaining-filter*)	; Because special variables are slower than lexical variables:  just an optimization.
	(predicate *default-schema-chaining-predicate*))	; Ditto.
    (loop for i from start-from below *schema-number*
	  for context-schema = (get-schema i)
	  when (and (not (schema-context-empty-p context-schema))
 		    (funcall filter    context-schema)
		    (funcall predicate context-schema result-schema))
	    do (return i))))

;;; Marks the current schema and recursively extends the chain, if it can.  While
;;; extending the chain is depth-first, we also go breadth-first in finding other
;;; schemas that might extend the chain from this schema for the case where this
;;; schema's result might chain to more than one other schema at this level (e.g., if
;;; our schema is A/FOO/B, then we'll get both B/BAR/C and B/BAZ/D, where we have
;;; two different actions leading from the same context to [same or different]
;;; results [C might or might not equal D]).
(defun EXTEND-SCHEMA-CHAIN (index)
  (let ((schema (get-schema index)))
    (mark-schema schema)
    (loop with low = 0
	  for next-index = (find-next-chained-schema low schema)
	  while next-index
	  when next-index			; Start searching from here next time, marked or unmarked.
	    do (setf low (1+ next-index))	; [E.g., this plus the WHEN below will just skip over marked schemas.]
;              (princ ".")			; DBG.
	       (setf (schema-chain schema) (push next-index (schema-chain schema)))	; Add the new index to the chain from this schema.
	       (when (not (schema-marked? (get-schema next-index))) ; Only attempt to elaborate the chain if unmarked.
;                (princ "+")			; DBG.
		 (extend-schema-chain next-index))))  ; Search recursively (depth-first) for extensions of _that_ chain.
  (values))

;;; The toplevel for building all the chains.  Returns the root set, from which all
;;; chains may be derived.  [This took about 20 minutes to run with the 376-long
;;; root set (e.g., the old semireliable root set).  Dunno how long it'll take to run
;;; with the complete ~1200 schema root set.]
(defun SCHEMA-CHAINING-ROOT ()
  (reset-schema-chaining)
  (let* ((root-set (schema-chaining-root-set))
	 (length   (length root-set)))
    (noting-progress ("Building schema chaining roots")
      (loop for index in root-set
	    for counter from 1
	    do (note-progress counter length)
	       (extend-schema-chain index)))
    (reset-schema-marks)
    root-set))

(defvar *PRESERVED-SCHEMA-CHAINING-ROOTS* nil)

;;; This only takes about half a second to run.
(defun PRESERVE-SCHEMA-CHAINING-ROOTS ()
  (let ((table (make-hash-table)))
    (dotimes (i *schema-number*)
      (let ((chain (schema-chain (get-schema i))))
	(when chain
	  (setf (gethash i table) chain))))
    (push table *preserved-schema-chaining-roots*)
    table))

;;; This takes about 200 ms to run.
(defun RESTORE-SCHEMA-CHAINING-ROOTS (&optional (table (car *preserved-schema-chaining-roots*)))
  (unless (hash-table-p table)			; Check first, before bashing possibly-unpreserved state.
    (error "Sorry, don't have a hash table to restore from (got ~S)." table))
  (reset-schema-chaining)
  (maphash #'(lambda (key value)
	       (setf (schema-chain (get-schema key)) value))
	   table)
  (values))

;;;; Making use of the generated set of chains.

;;; Note that ordinary depth-first is not the right way to do this, because it tends to
;;; find the longest path first.  So we modify the general idea a little and use
;;; iterative deepening, which seems to work _very_ well!  (It's much, much better
;;; than the A* implementation later in this file.)

;;; Produces a list of actions that follows a chain of schemas, starting from one
;;; schema and continuing until we either reach the end of the chain, or until we get
;;; to the ending schema.  In the first case, we return a second value of NIL, and in
;;; the second case, T.  The list is sorted backwards (e.g., the first item in the list is
;;; the goal, and the last item is the starting state), because we generate it with
;;; PUSH.
;;;
;;; This would be trivial except for the case of bifurcations, which are common.
;;; What we wind up doing is walking down the tree depth-first, marking as we go,
;;; and backtracking if we hit a mark without having hit the target yet.

(defun PRECHECK-RELEVANCY (start end)
  (loop for i in (list start end)
	unless (schema-index-relevant? i)
	  do (error "Sorry, ~D fails the relevancy test, so therefore there cannot be a chain which involves it."
		    i)))

;;; See comment at FIND-ALL-INITIALS-FROM-FINALS.
(defun DEPTH-FIRST-GENERATE-ACTION-SEQUENCE (starting-schema-index ending-schema-index &key
					     (depth-limit most-positive-fixnum)
					     dont-mention-reachability assume-reachability)
  (precheck-relevancy    starting-schema-index ending-schema-index)	; Bitch even if ASSUME-REACHABILITY is true.  Policy decision.
  (precheck-reachability starting-schema-index ending-schema-index dont-mention-reachability assume-reachability)
  (reset-schema-marks)
  (depth-first-generate-action-sequence-1 starting-schema-index ending-schema-index nil depth-limit))

;;; See comment at FIND-ALL-INITIALS-FROM-FINALS.
(defun DEPTH-FIRST-GENERATE-ACTION-SEQUENCE-1 (start end indices-so-far depth-limit)
  (cond ((eql start end)
	 (values (push end indices-so-far) t))
	(t
	 (when (< (length indices-so-far) depth-limit)	; $OPT:  This would be faster to compute if we tracked the length along with the result...
	   (let* ((schema (get-schema start))	; ... or if we just decremented DEPTH-LIMIT for each recursive call.
		  (next-list (schema-chain schema)))
	     (cond ((schema-marked? schema)	; If the current schema is marked, but wasn't the target, this branch fails.
		    (pop indices-so-far)
		    (values indices-so-far nil))
		   (t
		    (mark-schema schema)
		    (push start indices-so-far)
		    (loop for next in next-list
			  when (schema-index-relevant? next)	; Make sure this is within the schemas we're allowed to remember (lobotomy simulator).
			    do (multiple-value-bind (actions success?)
				   (depth-first-generate-action-sequence-1 next end indices-so-far depth-limit)
				 (when success?
				   (return-from depth-first-generate-action-sequence-1
				     (values actions t)))))
		    (pop indices-so-far)
		    (values indices-so-far nil))))))))

;;; See comment at FIND-ALL-INITIALS-FROM-FINALS.
(defun ITERATIVE-DEPTH-FIRST-GENERATE-ACTION-SEQUENCE (start end &key
							 (depth-limit most-positive-fixnum)
							 (dont-mention-reachability t)
							 assume-reachability)
  (precheck-relevancy    start end)		; Bitch even if ASSUME-REACHABILITY is true.  Policy decision.
  (precheck-reachability start end dont-mention-reachability assume-reachability)	; Check just once, not every time through the loop.
  (loop for depth from 0 below depth-limit
	do ;; (format t "~&Depth ~D~&" depth)	; DBG.
	   (multiple-value-bind (results success?)
	       (depth-first-generate-action-sequence
		 start end
		 :depth-limit depth
		 :assume-reachability t)	; We've already checked above.
	     (when success?
	       (return-from iterative-depth-first-generate-action-sequence results)))))

;;; A debugging function.  NOTE!  This won't return useful results unless you've run
;;; RECONSTRUCT-SCHEMA-CHAINING-ROOT-AND-REACHABILITY-CACHES first!  (Or at
;;; least SCHEMA-CHAINING-ROOT!)
(defun CHECK-ITERATIVE-DEPTH-FIRST-FOR-SCHEMAS (source &optional
						(target-range-start 0)
						(target-range-limit *schema-number*))
  (loop for i from target-range-start below target-range-limit
	when (schema-reachable? source i)
	  do (format t "~&~4D:  " i)		; Done as two separate calls, so I can see where it is if it pauses or loops.
	     (let* ((start-time (get-universal-time))	; Bindings must be done sequentially for this to be meaningful.
		    (result (iterative-depth-first-generate-action-sequence
			      source i
			      :assume-reachability t)))	; By definition, we know that anything we ask it about will be reachable.
	       (format t "[~3D]  ~S~&"
		       (- (get-universal-time) start-time)
		       result)))
  (values))

;;; Also a debugging function.  Letting this run to completion is the wrong idea.
(defun CHECK-ITERATIVE-DEPTH-FIRST-FOR-ALL-SCHEMAS (&optional (source-range-start 0)
						    (source-range-limit *schema-number*)
						    (target-range-start 0)
						    (target-range-limit *schema-number*))
  (loop for i from source-range-start below source-range-limit
	do (check-iterative-depth-first-for-schemas i target-range-start target-range-limit))
  (values))

;;;; A* search stuff.  Stolen from _The Art of AI_, around page 210 or so.

;;; Note that this stuff, while theoretically working (and upon which I wasted quite a
;;; bit of time determining that it was robust in the face of loops, etc, before trying
;;; the obvious tests [in CHAINING-DEBUGGING.LISP] that show that it is), is too slow.
;;; Because we don't have a heuristic function available to guide the search, it takes
;;; minutes to an hour or more to search the space, and conses tens of megawords
;;; of intermediate trash (in the form of PATH structures and lists of them).  It's left
;;; here in case I need an A* search elsewhere, and because little stubs and other
;;; glue logic written to support this is useful elsewhere in this file.

;;;; +++ The basic A* stuff.

(defstruct (PATH
	     (:PRINT-FUNCTION PRINT-PATH))
  state
  (previous nil)
  (cost-so-far 0)
  (total-cost 0))

(defun PRINT-PATH (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~A cost ~,1F>"
	  (path-state path)
	  (path-total-cost path)))

(defun INDICES->SCHEMA-NAMES (indices)
  (mapcar #'(lambda (index)
	      (schema-print-name (get-schema index)))
	  indices))

(defun INDICES->SCHEMA-RELIABILITY-RANGES (indices)
  (mapcar #'(lambda (index)
	      (let ((schema (get-schema index)))
		(if (reliable-schema? schema)
		       :r :u)))
	  indices))

(defun FIND-PATH (state paths state=)
  (find state paths :key #'path-state :test state=))

(defun BETTER-PATH (path1 path2)
  (< (path-total-cost path1)
     (path-total-cost path2)))

(defun INSERT-PATH (path paths)
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun PATH-STATES (path)
  (when path
    (cons (path-state  path)
	  (path-states (path-previous path)))))

;;; Note that this won't infinite-loop, even on a cyclic graph.
(defun A*-SEARCH (paths goal-p successors cost-fn cost-left-fn &optional
		  (state= #'eql) old-paths)
  (cond ((null paths)
	 nil)					; Just return NIL (e.g., no path).
	((funcall goal-p (path-state (first paths)))
	 (values (first paths) paths))
	(t
	 (let* ((path (pop paths))
		(state (path-state path)))
	   ;; Update PATHS and OLD-PATHS to reflect the new successors of STATE.
	   (setf old-paths (insert-path path old-paths))
	   (dolist (state2 (funcall successors state))
	     (let* ((cost (+ (path-cost-so-far path)
			     (funcall cost-fn state state2)))
		    (cost2 (funcall cost-left-fn state2))
		    (path2 (make-path
			     :state state2
			     :previous path
			     :cost-so-far cost
			     :total-cost (+ cost cost2)))
		    (old))
	       ;; Place the new path, PATH2, in the right list.
	       (cond ((setf old (find-path state2 paths state=))
		      (when (better-path path2 old)
			(setf paths (insert-path path2 (delete old paths)))))
		     ((setf old (find-path state2 old-paths state=))
		      (when (better-path path2 old)
			(setf paths (insert-path path2 paths))
			(setf old-paths (delete old old-paths))))
		     (t
		      (setf paths (insert-path path2 paths))))))
	   ;; Finally, call A* again with the updated path lists.
	   (a*-search paths goal-p successors cost-fn cost-left-fn state= old-paths)))))

;;;; +++ Using the A* stuff to search with.

;;; This just returns 1, so path costs are strictly related to their lengths.
;;; It could be replaced in the future by something that, for instance, said
;;; that a path involving moving the eye was less costly than a path involving
;;; moving the hand, to specify a policy for which way we're like some goal
;;; to be achieved (e.g., would we like to move the hand to the fovea, or move
;;; the fovea to the hand?).
(defun A*-PATH-LENGTH-COST-FUNCTION (&rest ignore)
  ignore					; Ignored.  (Stupid Harlequin compiler...)  [&&& Note that this will cons to make IGNORE anyway!]
  1)

;;; For the moment, this just returns 0 all the time, which amounts to disabling
;;; any heuristics on which paths might be shorter (e.g., we can't hillclimb in
;;; schema space).  This might be replaced in the future by something that
;;; attempted to implement either policy or hints about how to proceed.
(defun A*-HEURISTIC-EVALUATOR (&rest ignore)	; $OPT:  Can I make this a subst?
  ignore					; Ignored.  (Stupid Harlequin compiler...)  [&&& Note that this will cons to make IGNORE anyway!]
  0)

(defun A*-SCHEMA-SUCCESSORS (index)		; $OPT:  Can I make this a subst?
  (schema-chain (get-schema index)))

(defun IS (value)				; $OPT:  Can I make this a subst?
  #'(lambda (x)
      (eql x value)))

(defun PRECHECK-REACHABILITY (start end dont-mention-reachability assume-reachability)
  (unless assume-reachability			; Don't waste eons searching for a path that isn't there.
    (unless (schema-reachable? start end)
      (error "Sorry, schema ~D [~A] isn't reachable from ~D [~A]."
	     end   (schema-print-name (get-schema end))
	     start (schema-print-name (get-schema start))))
    (unless dont-mention-reachability
      (format t "~&Reachable.  Searching paths...~&"))))

(defun A*-GENERATE-ACTION-SEQUENCE (start end &optional	  ; Indices.
				    dont-mention-reachability assume-reachability)
  (precheck-relevancy    start end)		; Bitch even if ASSUME-REACHABILITY is true.  Policy decision.
  (precheck-reachability start end dont-mention-reachability assume-reachability)
  (path-states
    (a*-search (list (make-path :state start))
	       (is end)
	       #'a*-schema-successors
	       #'a*-path-length-cost-function
	       #'a*-heuristic-evaluator)))

;;;; Discovering islands of competency, kinda.

;;; +++ This is defined here so it can be saved, restored, and cleared with the other
;;; +++ stuff on this page, but it's not actually used until CHAINING-TO-GOALS.LISP.
(defvar *CACHED-INITIALS-TO-FINALS* (make-hash-table))
;;; ---

(defun MARK-REACHABLE-SCHEMAS-1 (start)
  (let ((schema (get-schema start)))
    (unless (schema-marked? schema)
      (mark-schema schema)
      (loop for next in (schema-chain schema)
	    when (schema-index-relevant? next)
	      do (mark-reachable-schemas-1 next)))))

(defun ALL-MARKED-SCHEMA-INDICES ()
  (loop for i from 0 below *schema-number*
	when (schema-marked? (get-schema i))
	  collect i))

(defun REACHABLE-SCHEMAS-INTERNAL (start)
  (reset-schema-marks)
  (mark-reachable-schemas-1 start))

(defun REACHABLE-SCHEMAS (start)
  (reachable-schemas-internal start)
  (all-marked-schema-indices))

(defvar *CACHED-REACHABLE-SCHEMAS* (make-hash-table))

;;; For ensuring that the cache matches reality.  We might still have changed the relevancy
;;; function by recompiling it to check something else, but this way at least changing its
;;; variable limits will warn us if it invalidates the cache.
(defvar *LAST-SCHEMA-RELEVANCY-LIMIT-LOW*  *schema-relevancy-limit-low*)
(defvar *LAST-SCHEMA-RELEVANCY-LIMIT-HIGH* *schema-relevancy-limit-high*)

;;; Dangerous!  Flushes the cache.  Warns unless DONT-WARN set.
(defun RESET-CACHE-RELEVANCY (dont-warn)
  (when (or dont-warn
	    (not (preserved-caches-bash-query)))     ; Don't worry about the roots, since we're not touching them.
    (flush-reachability-cache t)
    (setf *last-schema-relevancy-limit-low*  *schema-relevancy-limit-low*)
    (setf *last-schema-relevancy-limit-high* *schema-relevancy-limit-high*))
  (values))

(defun CACHED-REACHABLE-SCHEMAS (start)
  (unless (and (= *last-schema-relevancy-limit-low*  *schema-relevancy-limit-low*)
	       (= *last-schema-relevancy-limit-high* *schema-relevancy-limit-high*))
    (error "Relevancy limits changed since the cache was built.  Reset them & toss the cache."))
  (or (gethash start *cached-reachable-schemas*)
      (setf (gethash start *cached-reachable-schemas*)
	    (reachable-schemas start))))

;;; This is a reasonable thing to run to preserve the cache when changing
;;; reachability criteria.  Note that it copies only the toplevel table structure, and
;;; not the individual lists.  That's sufficient to recreate the table if it's cleared,
;;; assuming that nobody's still got their hands on one of the individual lists and
;;; starts modifying it (though no current code does this).
(defun COPY-CHAINING-CACHE (cache)
  (let ((new (make-hash-table)))
    (maphash #'(lambda (key value)
		 (setf (gethash key new) value))
	     cache)
    new))

(defvar *SAVED-REACHABILITY-CACHES* nil)
(defvar *SAVED-INITIAL-FINAL-CACHES* nil)

(defun PRESERVE-REACHABILITY-CACHE ()
  (push (copy-chaining-cache *cached-reachable-schemas*)  *saved-reachability-caches*)
  (push (copy-chaining-cache *cached-initials-to-finals*) *saved-initial-final-caches*))

;;; This is required if I change my idea of what validly constitutes a search path.  If I
;;; didn't flush the cache, then the reachability checks that make sure we can get
;;; anywhere before wasting time looking for a short route would malfunction.  See
;;; the comments at COUNT-CACHED-REACHABLE-SCHEMAS-FROM-ROOT-SET for timing,
;;; but it's about 12 minutes at least to regenerate it.
(defun FLUSH-REACHABILITY-CACHE (&optional dont-save)
  (unless dont-save
    (preserve-reachability-cache))
  (clrhash *cached-reachable-schemas*)
  (clrhash *cached-initials-to-finals*))

(defun PRESERVE-ROOTS-AND-CACHES ()
  (preserve-schema-chaining-roots)
  (preserve-reachability-cache)
  (values))

;;; Dangerous!  Doesn't query, just smashes them.  Useful if you know the data is
;;; already preserved (say, because we just booted & read it in) and are swapping
;;; datasets.
(defun FLUSH-ROOTS-AND-CACHES ()
  (flush-reachability-cache t)
  (reset-schema-chaining)
  (values))

;;; This gets called from CLEAR-ALL-NONMETERS-PRE-MICROWORLD-INIT.
(def-microworld-independent-init PRESERVE-THEN-FLUSH-ROOTS-AND-CACHES ()
  ;; If even the first element of *SCHEMA-ARRAY* doesn't have a schema in it yet,
  ;; then this is the first time we've been called since cold-boot.  In that case, not
  ;; only can we confidently say that we don't have any caches (hence we
  ;; shouldn't try to preserve them, which would only result in shoving some NILs
  ;; on our preservation lists), but said preservation will actually malfunction
  ;; (because, e.g., PRESERVE-SCHEMA-CHAINING-ROOTS will try to find the CHAIN of
  ;; NIL, etc).
  (when (aref *schema-array* 0)
    (preserve-roots-and-caches)
    (flush-roots-and-caches)))

(defun SHOW-PRESERVED-ROOTS-AND-CACHES ()
  (format t "~&~S -> ~S~&~S -> ~S~&~S -> ~S~&"
	  '*preserved-schema-chaining-roots* *preserved-schema-chaining-roots*
	  '*saved-reachability-caches* *saved-reachability-caches*
	  '*saved-initial-final-caches* *saved-initial-final-caches*)
  (values))

(defun PRESERVED-CACHES-BASH-QUERY ()
  (let ((reachable
	  (when (and (hash-table-p *cached-reachable-schemas*)
		     (not (zerop (hash-table-count *cached-reachable-schemas*))))
	    (format t "~&~S appears to already have data in it.~&"
		    '*cached-reachable-schemas*)
	    t))
	(initial-final
	  (when (and (hash-table-p *cached-initials-to-finals*)
		     (not (zerop (hash-table-count *cached-initials-to-finals*))))
	    (format t "~&~S appears to already have data in it.~&"
		    '*cached-initials-to-finals*)
	    t)))
    (or reachable initial-final)))

(defun PRESERVED-ROOTS-BASH-QUERY ()
  (let ((count (count-if #'(lambda (schema)	; Unfortunately, even FLET with an inline declaration doesn't result in this being inlined, so the hell with it.
			     (schema-chain schema))	; Alas, SCHEMA-CHAIN was built with DEFMACRO and isn't a function.
			 *schema-array*
			 :end *schema-number*)))
    (unless (zerop count)
      (format t "~&The schemas already have ~D chain~:P in them.~&"
	      count)
      t)))

;;; Returns T if continuing would bash state.
(defun PRESERVED-ROOTS-AND-CACHES-BASH-QUERY ()
  (when (or (preserved-caches-bash-query)
	    (preserved-roots-bash-query))
    (format t "~&Clear the appropriate data, possibly saving it first, and try again.~&")
    t))

;;; Puts stuff back in place from the data we read.  If BASH-QUERY is non-NIL, and it
;;; looks like we'll be bashing existing data, punts and lets us save it in some other
;;; way first.  Since this'll change the index into the list (albeit in a predictable
;;; way), we just punt, instead of doing it all.  Note that this only takes about 200 ms,
;;; if everything's paged in already.
(defun SELECT-PRESERVED-ROOTS-AND-CACHES (n &optional (bash-query t))
  (assert (and (numberp n)
	       (not (minusp n))
	       (< n (length *preserved-schema-chaining-roots*))
	       (= (length *preserved-schema-chaining-roots*)
		  (length *saved-reachability-caches*))))
  (unless (and bash-query
	       (preserved-roots-and-caches-bash-query))
    (setf *cached-reachable-schemas*  (nth n *saved-reachability-caches*))
    (setf *cached-initials-to-finals* (nth n *saved-initial-final-caches*))
    (restore-schema-chaining-roots    (nth n *preserved-schema-chaining-roots*)))
  (values))

;;; This took about 18 minutes (with a bad guess [e.g., only 3000 blocks] for the amount of
;;; FEP preallocation to do), no consing, and 8033 FEP blocks!  Saved data looked like this:
;;; *preserved-schema-chaining-roots* -> (#<Table :TEST EQL 1937/4084 150000177> #<Table :TEST EQL 4372/6872 1401545361>)
;;; *saved-reachability-caches* ->            (#<Table :TEST EQL 1226/2404 150113322> #<Table :TEST EQL 615/1396 1401545336>)
(defun WRITE-ROOTS-AND-CACHES (&key (and-current t)	; E.g., snapshot the current ones before saving.
			       (pathname "FEP1:>Snap>roots-and-caches.ibin"))
  (when and-current
    (preserve-roots-and-caches))
  ;; There are probably lots of shared lists here, but during the dump is NOT the
  ;; time I want to compute this!  Furthermore, then I won't be able to change them
  ;; after reloading (e.g., we'll have seriously changed the semantics if a reload
  ;; intervenes).
  (dump-variables-preallocating
    (* 640 8000)
    pathname
    *preserved-schema-chaining-roots*
    *saved-reachability-caches*
    *saved-initial-final-caches*))		; Dumps before Schema 12.54, 10 Apr 94, won't have this one.

;;; This is a toplevel of sorts; use it when changing the criteria by which schemas
;;; may chain (e.g., chains of all schemas vs chains of only reliable schemas, etc).
(defun RECONSTRUCT-SCHEMA-CHAINING-ROOT-AND-REACHABILITY-CACHES ()
  (format t "~&Reconstructing roots...~&")
  (preserve-schema-chaining-roots)
  (schema-chaining-root)
  (format t "~&Reconstructing reachability cache...~&")
  (flush-reachability-cache)
  (count-cached-reachable-schemas-from-root-set)
  (values))

;;; [This took about 37 minutes for #'RELIABLE-SCHEMA, for the full ~1200 schema
;;; root set, and including probably 10-15 minutes of arrest for (slow! optical!) GC.]
(defun RECONSTRUCT-SCHEMA-CHAINING-ROOT-AND-REACHABILITY-CACHES-FOR-FILTER (filter)
  (let ((*default-schema-chaining-filter* filter))
    (reconstruct-schema-chaining-root-and-reachability-caches)))

;;; Surely this would be better placed in some other file?
(defun NAME->SCHEMA (name)
  (find name *schema-array*
	:test #'(lambda (name schema)
		  (string-equal name (schema-print-name schema)))))

;;; Surely this would be better placed in some other file?
(defun NAME->SCHEMA-POSITION (name)
  (position name *schema-array*
	    :test #'(lambda (name schema)
		      (string-equal name (schema-print-name schema)))))

;;; Synonym for above, since I can never remember it.
(defsubst SCHEMA-NAME->INDEX (name)
  (name->schema-position name))

(defun NAME->REACHABLE-SCHEMAS (name &optional verbose)
  (let ((index (name->schema-position name)))
    (assert (not (null index)))
    (when verbose
      (format t "~&Chaining from schema index ~D...~&" index))
    (cached-reachable-schemas index)))

(defun SCHEMA-REACHABLE? (start end)
  (find end (cached-reachable-schemas start)))

;;; E.g., (schema-substrings-reachable-from 2599 "hp")).
;;; But we want it the other way around...
(defun SCHEMA-SUBSTRINGS-REACHABLE-FROM-INDEX (start substring)
  (loop for i from 0 below *schema-number*
	for name = (schema-print-name (get-schema i))
	when (and (search substring name)
		  (schema-reachable? start i))
	  collect i))

(defun SCHEMA-INDEX-REACHABLE-FROM-SUBSTRINGS (start substring)
  (loop for i from 0 below *schema-number*
	for name = (schema-print-name (get-schema i))
	when (and (search substring name)
		  (schema-reachable? i start))	; This is the changed line.
	  collect i))

(defun COUNT-SCHEMAS-REACHABLE-FROM-SUBSTRING (substring &optional
					       (range-low 0)
					       (range-limit *schema-number*))
  (let ((results nil))
    (loop for i from range-low below range-limit
	  for name = (schema-print-name (get-schema i))
	  when (search substring name)
	    do (let ((count (length (cached-reachable-schemas i))))
		 (format t "~&~D [~A] reaches ~D other~:P.~&"
			 i name count)
		 (push (list i count) results)))
    results))

;;; Shows that A* can find paths, albeit quite slowly (2-50 minutes!),
;;; and with lots of consing.
(defun CHECK-A*-FOR-SCHEMAS (source &optional
			     (target-range-start 0)
			     (target-range-limit *schema-number*))
  (loop for i from target-range-start below target-range-limit
	when (schema-reachable? source i)
	  do (format t "~&~4D:  " i)		; Done as two separate calls, so I can see where it is if it pauses or loops.
	     (let* ((start-time (get-universal-time))	; Bindings must be done sequentially for this to be meaningful.
		    (result (a*-generate-action-sequence source i)))
	       (format t "[~3D]  ~S~&"
		       (- (get-universal-time) start-time)
		       result)))
  (values))

;;; This version caches the intermediate lists, meaning that it conses a fair amount.
;;; Useful if we're going to then compare the islands, but not if we just want a count.
;;; [This took about 12 minutes to run on a root set of 376 schemas (the old semireliable
;;; root set).  I dunno how long it would take on the ~1200 schema complete root set.]
(defun COUNT-CACHED-REACHABLE-SCHEMAS-FROM-ROOT-SET (&optional (root-set (schema-chaining-root-set)))
  (loop for root in root-set
	for reachable = (cached-reachable-schemas root)
	do (format t "~&~4D:  ~D~&"
		   root
		   (length reachable)))
  (values))

;;; This turns out not to work so well:  Every sequence starts with the individual root,
;;; so they're all different.
(defun PARTITION-CACHED-REACHABLE-SCHEMAS ()
  (let ((table (make-hash-table :test #'equal)))
    (maphash #'(lambda (key value)		; E.g., 15 and a long list.
		 (multiple-value-bind (found-value found?)
		     (gethash value table)	; Look up the long list in our new table.
		   (setf (gethash value table)
			 (if found?
			     (push key found-value)
			     (list key)))))
	     *cached-reachable-schemas*)
    table))

;;; This one ignores the leading piece of the sequence.  It's better, but only by about 50% or so.
;;; (Still many, many overlapped sequences, of course.)
(defun PARTITION-CACHED-REACHABLE-SCHEMAS-1 ()
  (let ((table (make-hash-table :test #'equal)))
    (maphash #'(lambda (key whole-value)	; E.g., 15 and a long list.
		 (let ((value (cdr whole-value)))	; Punt first step.
		   (multiple-value-bind (found-value found?)
		       (gethash value table)	; Look up the long list in our new table.
		     (setf (gethash value table)
			   (if found?
			       (push key found-value)
			       (list key))))))
	     *cached-reachable-schemas*)
    table))

;;; This version doesn't actually generate any lists.  Doesn't cons, and runs
;;; faster than the list version, unless the cache has already been generated.
;;; [We do, however, return a list of the resulting statistics, for later use.]
(defun COUNT-REACHABLE-SCHEMAS-FROM-ROOT-SET (&optional (root-set (schema-chaining-root-set)))
  (let ((results nil))
    (loop for root in root-set
	  do (reachable-schemas-internal root)
	     (let ((count (count-marked-schemas)))
	       (format t "~&~4D:  ~D~&"
		       root
		       count)
	       (push (list root count) results)))
    results))

(defvar *REACHABILITY-RESULTS* nil)

(defun SUMMARIZE-REACHABILITY-RESULTS ()
  (setf *reachability-results*
	(or *reachability-results*
	    (count-reachable-schemas-from-root-set)))
  (let ((table (make-hash-table)))
    (loop for (root count) in *reachability-results*
	  do (multiple-value-bind (value found?)
		 (gethash count table)
	       (setf (gethash count table)
		     (if found?
			 (push root value)
			 (list root)))))
    table))

;;; +++ Debuggery (kinda like a progress report, for things that do their work
;;; +++ by marking schemas, but do it slowly.)

;;; You could get the same effect by doing (LENGTH (ALL-MARKED-SCHEMA-INDICES)),
;;; but this one won't cons.  It also runs about three times faster.
(defun COUNT-MARKED-SCHEMAS ()
  (flet ((marked? (s)
	   (schema-marked? s)))			; SCHEMA-MARKED is a macro (so I can use SETF on it), not a subst.
    (count-if #'marked?
	      *schema-array*
	      :end *schema-number*)))

(defun PERIODICALLY-COUNT-MARKED-SCHEMAS (&optional (period 2))	; Seconds.
  (terpri)
  (loop doing
    (format t "~D " (count-marked-schemas))
    (sleep period)))

;;;; Automatically generating roots and (maybe?) caches at various stages of knowledge.

(defparameter *NUMBER-OF-KNOWLEDGE-STEPS* 10)	; How many different levels of knowledge.
(defparameter *KNOWLEDGE-STEPS-INCLUDE-ALL* t)	; We may already have statistics from the largest run seen, so allow not including it.

(defun SCHEMA-NUMBERS-FROM-STEPS (&key (steps *number-of-knowledge-steps*)
				  (include-all *knowledge-steps-include-all*)
				  (low 0)
				  (high *schema-number*))
  (let* ((range-size
	   (- high low))
	 (chunk
	   (/ range-size steps))
	 (steps
	   (loop for multiplier from 1 to steps
		 collect (+ low (floor (* multiplier chunk))))))
    (if (and include-all
	     (/= high *schema-number*))
	(append steps (list *schema-number*))
	steps)))

;;; OOPS.  Right, there's no sense recomputing the chain; we just want to notice it
;;; when searching.  So this is unnecessary.
;;;
;;; You'd better destroy and/or save any info before you run this.
; (defun GENERATE-ROOTS-FROM-STEPS ()
;   (preserved-roots-and-caches-bash-query)	; Don't allow destroying data.
;   (loop for fake-schema-number in (schema-numbers-from-steps)
;       do (let ((*schema-relevancy-limit-high* fake-schema-number))
;            (schema-chaining-root)
;            (format t "~&Computing for a fake *SCHEMA-NUMBER* of ~D...~&" fake-schema-number)	; DBG.
;            (reset-cache-relevancy t)   ; Reset the cache, even if I don't do the COUNT- below, in case somebody else starts asking about reachability.
; ;            (count-cached-reachable-schemas-from-root-set)	; I dunno if I need this except when we're actually hunting for a chain...
;            (preserve-roots-and-caches)
;            ))
;   (reset-cache-relevancy t)			; Do it again here, just so I don't screw up later.
;   (values))

(defmacro WITH-RELEVANCY-CACHE-INVALIDATED (&body body)
  `(unwind-protect
       (progn
	 ,@body)
     (reset-cache-relevancy t)))

(defun INDEX->SCHEMA-NAME (index)
  (schema-print-name (get-schema index)))

;;; We push on tuples of (start end fake-schema-number path).
(defvar *LOBOTOMY-RESULTS* nil)			; For later analysis.

(defun FIND-PATHS-WITH-LOBOTOMIES (start end	; Schema indices.
				   &key (steps *number-of-knowledge-steps*)
				   (include-all *knowledge-steps-include-all*)
				   (low 0)
				   (high *schema-number*)
				   only-successes
				   ignore-shortest-path
				   show-shortest-if-others	; Only if IGNORE-SHORTEST-PATH non-NIL.
				   dont-save-results)
  (cond ((not (schema-reachable? start end))	; Make sure it's reachable in the first place.
	 (format t "~&~D [~A] isn't reachable from ~D [~A] even with all schemas.~&"
		 start (index->schema-name start)
		 end   (index->schema-name end)))
	(t
	 (let* ((all-steps
		  (schema-numbers-from-steps
		    :steps steps
		    :include-all include-all
		    :low low
		    :high high))
		(undamaged-path
		  (iterative-depth-first-generate-action-sequence start end))
		(max-index-in-path
		  (maximize undamaged-path))
		(pruned-steps
		  (if ignore-shortest-path
		      (loop for step in all-steps
			    when (< step max-index-in-path)
			      collect step)))
		(showed-a-path nil))
	   (format t "(Max in shortest ~D [~A], length ~D)  "
		   max-index-in-path
		   (index->schema-name max-index-in-path)
		   (length undamaged-path))
	   (with-relevancy-cache-invalidated
	     (loop for fake-schema-number in pruned-steps
		   do (let ((*schema-relevancy-limit-high* fake-schema-number))
			(reset-cache-relevancy t)
			(cond ((or (> start fake-schema-number)
				   (> end   fake-schema-number))
			       (unless only-successes
				 (format t "~&Skipping step at ~D; at least one chain end is already higher.~&"
					 fake-schema-number)))
			      (t
			       (cond ((schema-reachable? start end)
				      (let ((path (iterative-depth-first-generate-action-sequence start end)))
					(format t "~&Fake *SCHEMA-NUMBER* = ~D:  [Length ~D] ~S"
						fake-schema-number
						(length path) path)
					(unless dont-save-results
					  (push (list start end fake-schema-number path) *lobotomy-results*))
					(setf showed-a-path t)))
				     (t
				      (unless only-successes
					(format t "~&Fake *SCHEMA-NUMBER* = ~D:  Unreachable.~&" fake-schema-number))))))))
	     (when (and ignore-shortest-path
			showed-a-path
			show-shortest-if-others)
	       (unless dont-save-results
		 (push (list start end max-index-in-path undamaged-path) *lobotomy-results*))
	       (format t "~&Fake *SCHEMA-NUMBER* = ~D:  [Length ~D (shortest)] ~S"
		       max-index-in-path
		       (length undamaged-path) undamaged-path))))))
  (values))

;;; It's tempting to find the path with all schemas, then only search with the fake
;;; limit set above the max of the path or something.  But this is erroneous:  the
;;; schema might be reachable through a longer path involving lower schemas.
;;;
;;; Note that, in the absence of CONTEXT-EQUAL, we just compare contexts by the
;;; pnames (ignoring syn-items etc).  This is cheesy, but sufficient for the moment.
(defun FIND-ALL-PATHS-WITH-LOBOTOMIES (start	; Schema index.
				       &key (steps *number-of-knowledge-steps*)
				       (only-successes t)
				       (prune-identical-contexts t))
  (format t "~&Starting from ~D [~A].~&"
	  start
	  (index->schema-name start))
  (let ((table (make-hash-table :test #'string-equal)))
    (let ((ends (cached-reachable-schemas start)))
      (loop for end in ends
	    when (or (not prune-identical-contexts)
		     (let ((context (subseq (index->schema-name end)
					    0 (search "/" (index->schema-name end)))))
		       (multiple-value-bind (value found?)
			   (gethash context table)
			 value			; Ignored.
			 (unless found?
			   (setf (gethash context table) t))
			 (not found?))))
	      do (format t "~&->->->Trying to reach ~D [~A]:~45T"
			 end
			 (index->schema-name end))
		 (find-paths-with-lobotomies
		   start end
		   :steps steps
		   :include-all nil		; By definition, we're only looking at schemas that we can reach with an undamaged brain.
		   :only-successes only-successes
		   :ignore-shortest-path t
		   ))))
  (values))

;;; End of file.
