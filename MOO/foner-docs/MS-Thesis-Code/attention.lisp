;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Spinning off schemas in _almost_ the old-fashioned way.

(in-package :schema)

;;; [ +++ The comment below only applies to NEW-MAYBE-SPINOFF-SCHEMA, _not_ the whole file!]

;;; This implementation is like the "old" (Ramstad) implementation semantically, if you set
;;; *MORE-THAN-ONE-SPINOFF-PER-ITERATION* NIL.  The loop is not "inside out".  In addition
;;; to allowing more than one spinoff per iteration if the above variable is non-NIL, this version
;;; uses iterators to control the loop, instead of the hardwired method of the old function.
;;; This version does _not_ do any loop metering.

;;; I'm leaving this version around mostly so it's possible to do a reference run with unischemas
;;; turned on, since all the other, "inside out" versions run with multischemas per iteration.

(defswitch *MORE-THAN-ONE-SPINOFF-PER-ITERATION* t more-than-one-spinoff-per-iteration)

(defmacro MAYBE-RETURN (&body body)
  `(cond (*more-than-one-spinoff-per-iteration*
	  ,@body)
	 (t
	  (return ,@body))))

(defun NEW-MAYBE-SPINOFF-SCHEMA ()
  (with-iterator (yield-schema-index-fn reset-schema-index-fn #'all-schema-numbers-generator)
    (with-iterator (yield-item-index-fn reset-item-index-fn #'all-item-numbers-generator)
      (do-iterator (yield-schema-index-fn reset-schema-index-fn schema-index)
	;; All of the possible ways out of the COND use RETURN or a DO-like RETURNFORM to hand back
	;; a spun-off schema.  As soon as the inner loop in either COND branch does so, the LET will
	;; therefore return non-NIL, and the RETURN that's at the other end of this WHEN will terminate
	;; the outer loop before it finishes.  [See MAYBE-RETURN for a recent modification to this...]
	(when
	  (let ((schema (get-schema schema-index))
		(array-index 0)
		(record-offset 0))
	    (macrolet ((counter-pinned? (counter-array)
			 `(fix= *counter-maximum*
				(counter-array-value
				  ,counter-array array-index record-offset)))
		       (counter-positive? (counter-array)
			 `(flag-truep
			    (counter-array-pos
			      ,counter-array array-index record-offset)))
		       (counter-pinned-and-positive? (counter-array)
			 `(and (counter-pinned? ,counter-array)
			       (counter-positive? ,counter-array)))
		       (next-counter ()
			 `(if (fix= *counter-record-max-offset* record-offset)
			      (setq record-offset 0
				    array-index (fix1+ array-index))
			      (setq record-offset (fix+ *counter-bits* record-offset)))))
	      (cond ((schema-result-empty-p schema)
		     (let ((result-pos        (schema-extended-result-pos schema))
			   (result-neg        (schema-extended-result-neg schema))
			   (result-pos-conj   (schema-extended-result-conj-pos schema))
			   (result-children   (schema-result-children schema))
			   (res-conj-children (schema-result-conj-children schema)))
		       (do-iterator (yield-item-index-fn reset-item-index-fn item-index)
			 (when (state-unknown-p
				 (state-array-get-state result-children item-index))
			   ;; This item isn't a (result) child of this schema, so it's legal to spin off a result schema.
			   (when (counter-pinned-and-positive? result-pos)
			     (maybe-return (make-spinoff-result schema-index item-index (make-state-on))))
			   (when (counter-pinned-and-positive? result-neg)
			     (maybe-return (make-spinoff-result schema-index item-index (make-state-off)))))
			 (when (and (fix< item-index *conj-number*)	; $OPT:  Combine these WHENs?
				    (flag-falsep
				      (flag-array-get-flag res-conj-children item-index)))
			   (when (counter-pinned-and-positive? result-pos-conj)
			     (maybe-return (make-spinoff-result-conj schema-index item-index))))
			 (next-counter))))
		    (t
		     ;; non-empty result so check for context spinoffs
		     (let ((ext-context      (schema-extended-context schema))
			   (context-children (schema-context-children schema))
			   (current-item     -1)
			   (current-state    (make-state-unknown)))
		       (do-iterator (yield-item-index-fn reset-item-index-fn item-index
							 (unless (state-eq (make-state-unknown) current-state)
							   (make-spinoff-context schema-index current-item current-state)))
			 (when (and (state-unknown-p
				      (state-array-get-state context-children item-index))
				    (counter-pinned? ext-context)
				    (or (state-unknown-p current-state)
					(item-generality-< (get-item item-index)
							   (get-item current-item))))
			   (setq current-state
				 (if (counter-positive? ext-context)
				     (make-state-on)
				     (make-state-off))
				 current-item item-index))
			 (next-counter)))))))
	  (maybe-return 'maybe-spinoff-schema-finished))))))

;;;; Spinning off schemas with a _limited_ form of focus-of-attention:  only noticing
;;;; items which have changed recently, and schemas which depend upon them.

(def-iterator-generator SPINOFF
  all-schema-numbers-generator
  all-item-numbers-most-specific-first-generator)

;;; &&&
;;; If I was feeling ambitious, I'd combine the four defs and the macro below
;;; into something that auto-accumulated them, then built the body of the macro.
;;; I don't think I'm feeling that ambitious today.
(def-vector-metering-counter-periodically-reported *SPINOFF-INNER-LOOP-COUNTERS*
						   "~:D total spinoff inner loop iteration~:P.")
(def-vector-metering-counter *SPINOFF-RESULT-COUNTERS*)
(def-vector-metering-counter *SPINOFF-RESULT-CONJ-COUNTERS*)
(def-vector-metering-counter *SPINOFF-CONTEXT-COUNTERS*)

(defmacro WITH-METERED-SPINOFFS (&body body)
  `(let ((.inner-loop-counter.  0)
	 (.result-counter.      0)
	 (.result-conj-counter. 0)
	 (.context-counter.     0))
     (prog1					; Just in case the body returns something useful...
       (macrolet ((metered-make-spinoff-result (schema item state)
		    `(progn
		       (incf .result-counter.)
		       (make-spinoff-result ,schema ,item ,state)))
		  (metered-make-spinoff-result-conj (schema conj)
		    `(progn
		       (incf .result-conj-counter.)
		       (make-spinoff-result-conj ,schema ,conj)))
		  (metered-make-spinoff-context (schema item state)
		    `(progn
		       (incf .context-counter.)
		       (make-spinoff-context ,schema ,item ,state))))
	 ,@body)
       (push .inner-loop-counter.  *spinoff-inner-loop-counters*)
       (push .result-counter.      *spinoff-result-counters*)
       (push .result-conj-counter. *spinoff-result-conj-counters*)
       (push .context-counter.     *spinoff-context-counters*)
       )))
;;; ---

;;; Convenience function, to keep the spinoff function to a screenful.
(defmacro WITH-SPINOFF-UTILITIES (&body body)
  `(macrolet ((counter-pinned? (counter-array)
		`(fix= *counter-maximum*
		       (counter-array-value
			 ,counter-array array-index record-offset)))
	      (counter-positive? (counter-array)
		`(flag-truep
		   (counter-array-pos
		     ,counter-array array-index record-offset)))
	      (counter-pinned-and-positive? (counter-array)
		`(and (counter-pinned? ,counter-array)
		      (counter-positive? ,counter-array)))
	      (item-not-in-state-children? (children item-index)    ; For RESULT- and CONTEXT-CHILDREN, which use state arrays.
		`(state-unknown-p
		   (state-array-get-state ,children ,item-index)))
	      (item-not-in-flag-children? (children item-index)	    ; For RESULT-CONJ-CHILDREN, which uses a flag array.
		`(flag-falsep
		   (flag-array-get-flag ,children ,item-index))))
     ,@body))

;;; +++

;;; This is part of a mechanism that allows R-I-O-MAYBE-SPINOFF-SCHEMA to know
;;; that it has already spun off a more-specific schema in some prior iteration in this
;;; particular call to R-I-O-M-S-S, and not to make a less-specific one.  This can only
;;; work, of course, if the spinoff item generator is one that yields item numbers in
;;; most-specific-first order.  We keep this around in a global variable so we don't
;;; have to cons it afresh on each call, and so we don't have to worry about handing
;;; it back to our caller to keep track of.
(defvar *CONTEXT-SPINOFF-MARKERS* nil)

;;; This initializes the contest spinoff marker bitvector.  It does so by creating it
;;; afresh if either it didn't already exist, or if its length isn't currently the same as
;;; *SCHEMA-MAXIMUM* (this allows us to change the latter without remember to
;;; change this, too---admittedly, a rare thing, but the overhead of one LENGTH on a
;;; bitvector and one equality comparison is lost in the nose compared to everything
;;; else that's about to happen).  If the bitvector _did_ already exist and was the
;;; right length, we just zero it.
(defsubst CLEAR-CONTEXT-SPINOFF-MARKERS ()
  (cond ((and *context-spinoff-markers*
	      (= (length *context-spinoff-markers*) *schema-maximum*))
	 (clear-bit-vector *context-spinoff-markers*))
	(t
	 (setf *context-spinoff-markers* (make-schema-bit-vector)))))

;;; Makes sure it's initialized, then binds it lexically, since lexical variable access is
;;; usually faster than global variable access in most implementations.  Picks a
;;; particular name for the lexical variable, without any choice by the caller.
(defmacro WITH-CONTEXT-SPINOFF-MARKERS (&body body)
  `(progn
     (clear-context-spinoff-markers)
     (let ((context-spinoff-markers *context-spinoff-markers*))
       ,@body)))

;;; ---

;;; +++

;;; This fixes a longstanding bug that has presumably been there forever in the
;;; Ramstad implementation, and, for all I know, in Drescher's!  The problem here is as
;;; follows.  Suppose we have a schema A/x/y, and then add another context item to
;;; it, to get A&B/x/y.  Suppose that this schema later becomes reliable.  This will
;;; cause us to turn A&B into a conj, correctly updating the schema to now have a
;;; conjunction as its context, yielding (A&B)/x/y.  So far, everything's copacetic.
;;; But suppose we later try to add B to the context of A/x/y again.  Since A&B/x/y
;;; doesn't exist any more (it was turned into (A&B)/x/y, e.g., a schema with a true
;;; conjunction as its context, instead of one whose context just happens to mention
;;; two items), we'll blithely go ahead and create a _second_ A&B/x/y!  This schema
;;; is a "duplicate" in that it has the same context items, even though it's not
;;; _quite_ a duplicate because it's got a different print-name.  Even worse, we
;;; might then create a "true" duplicate if that second schema, A&B/x/y, later
;;; becomes reliable---we'll find the original conj (A&B) (at least we won't create a
;;; second _conj_ with the same items!), but that code then assumes that it _can't_
;;; be duplicating a schema, so it will go ahead and transform that second schema
;;; into (A&B)/x/y, which definitely creates two schemas which are duplicates even
;;; in print-names.
;;;
;;; This only happens if the first schema has become reliable, of course.  If the first
;;; schema never became reliable, then its context will never have been mutated
;;; from A&B to (A&B), and we'd notice, when we later tried to turn A/x/y into
;;; A&B/x/y, that we were about to create a duplicate schema, and we'd punt.
;;;
;;; To avoid this problem, we check before adding an item to a schema's context
;;; array.  If the new context array would match an already-existing conjunction, we
;;; punt and do not create the schema.  This, plus the already-in-place mechanism in
;;; the previous paragraph (just the ordinary don't-create-duplicates logic), keeps
;;; us from creating duplicate schemas, regardless of whether "duplicate" means
;;; "same print-names" or "same context items".
;;;
;;; Note that a better implementation of this would not scan all conj's & schemas
;;; etc, but would keep the appropriate backpointers around to obviate the search.
;;; I'll do that if this becomes a major timesink, and any reimplementation should
;;; do that anyway.

;;; Implementation:

;;; This sees if there is a conj consisting of the item array, plus the new item.
;;; Since we're trying to compare the arrays word-by-word, this is tricky,
;;; since we can't just look for a word-by-word match & then add in the item:
;;; such a conj would have failed the word-by-word match first anyway.  So
;;; what we do is to manufacture a new item array that includes the new item,
;;; then do the fast search (word by word) for _that_.  To avoid consing, we
;;; keep the item array around between calls.  This is done in the same manner
;;; as for *CONTEXT-SPINOFF-MARKERS*.

;;; These are all substs for speed (e.g., no function-calling overhead).  Since they're
;;; only used in one place, the resulting code bloat is acceptable, and I'm done
;;; debugging them.

(defvar *CONJ-FIND-PLUS-ITEM-ARRAY* nil)

(defsubst CREATE-CONJ-FIND-PLUS-ITEM-ARRAY ()
  ;; Create the array unless it's there and the right size.
  (unless (and *conj-find-plus-item-array*
	       (= (length *conj-find-plus-item-array*) *fixna-required-to-hold-all-item-states*))
    (setf *conj-find-plus-item-array*
	  (make-state-array *fixna-required-to-hold-all-item-states*))))

(defsubst INIT-CONJ-FIND-PLUS-ITEM-ARRAY (item-array new-item-index new-item-state)
  (create-conj-find-plus-item-array)
  ;; Now stuff the right items into it:  the old item-array, and the new item.
  (state-array-copy item-array *conj-find-plus-item-array* *fixna-required-to-hold-all-item-states*)
  (setf (state-array-get-state *conj-find-plus-item-array* new-item-index) new-item-state)
  ;; ...and return it, for the convenience of our callers.
  *conj-find-plus-item-array*)

(defsubst CONJ-FIND-PLUS (item-array new-item-index new-item-state)
  (let ((item-array (init-conj-find-plus-item-array item-array new-item-index new-item-state)))
    (dotimes (x *conj-number* nil)
      (let ((compare-array (conj-item-array (get-conj x))))
	(when (dotimes (y *fixna-required-to-hold-all-item-states* t)	; $OPT:  Oh, ick.  This is basically a slow way to compare bit-arrays. ...
		(when (fix/= (aref compare-array y) (aref item-array y)); $OPT: ... It's faster than per-bit, but slower than Ivory block-array hardware.
		  (return nil)))
	  (return x))))))

;;; We've gotta check whether some other schema with the same action & result
;;; matches the conj we just found.  Otherwise, it's okay, because we can't create a
;;; duplicate anyway.
(defsubst MATCHING-SCHEMA-WITH-CONTEXT-CONJ-EXISTS? (query-schema conj-index)
  (let ((q-action (schema-action-item query-schema))
	(q-result (schema-result-item query-schema))
	(q-result-conj (schema-result-conj query-schema)))
    (loop for i from 0 below *schema-number*
	  do (let ((schema (get-schema i)))
	       (when (and (flag-truep (schema-context-conj schema))	; If it's not a conjuction, then it can't match the conjunction we've got.
			  (= (schema-context-item schema) conj-index))	; If it is a conjunction, make sure it matches the one we've got.
		 (let ((action (schema-action-item schema))	; Okay, it matches, so make sure the action & result match.
		       (result (schema-result-item schema))
		       (result-conj (schema-result-conj schema)))
		   (when (and (eql q-action action)
			      (eql q-result result)
			      (eql q-result-conj result-conj))
		     (return-from matching-schema-with-context-conj-exists? i)))))))
  nil)						; No match.

(defsubst SCHEMA-PLUS-ITEM-ALREADY-A-CONJ? (schema new-item-index new-item-state)
  (let ((conj (conj-find-plus (schema-context-array schema) new-item-index new-item-state)))
    (when conj					; Found some candidate conj; see if the rest of some schema with it matches ours.
      (matching-schema-with-context-conj-exists? schema conj))))

;;; ---

;;; +++

;;; Fix _another_ way to get into trouble with duplicate schemas.  This case isn't
;;; conj's, but it's closely analagous, and we use some of the same methods to solve
;;; it that we used above.
;;;
;;; Consider this scenario.  Given A&B/x/y and new item C, we create A&B&C/x/y
;;; (note that there are no true conj's in here; these schemas aren't reliable enough).
;;; Next, given B&C/x/y and new item A, we create A&B&C/x/y.  Oops!  Neither parent
;;; could have helped us avoid this, and we don't (until now) _search_ for A&B&C/x/y
;;; before creating it.  Now we do.

;;; Implementation:

;;; This steals use of *CONJ-FIND-PLUS-ITEM-ARRAY* from above (by calling
;;; INIT-CONJ-FIND-PLUS-ITEM-ARRAY), even though we're not looking for true
;;; conjunctions per se (e.g., we're looking for A&B&C, not (A&B&C)).  We can do this
;;; because we never try to use the array for both purposes at the same time, and
;;; this saves a little bit of code.  We also combine the two separate loops above
;;; (one that searches in the conj array, and one that searches in the schema array),
;;; since we're not searching true conj's, into one loop that looks for, e.g.,
;;; A&B&C/x/y all at once.

(defsubst FAKE-CONJ-FIND-PLUS (query-schema new-item-index new-item-state)
  (let ((query-context-item-array (schema-context-array query-schema)))
    (let ((item-array (init-conj-find-plus-item-array query-context-item-array new-item-index new-item-state))
	  (q-action (schema-action-item query-schema))
	  (q-result (schema-result-item query-schema))
	  (q-result-conj (schema-result-conj query-schema)))
      (loop for i from 0 below *schema-number*
	    do (let ((schema (get-schema i)))
		 (let ((action (schema-action-item schema))	; Check the fast cases first, to prune the maximal number of candidates up front.
		       (result (schema-result-item schema))
		       (result-conj (schema-result-conj schema)))
		   (when (and (eql q-action action)
			      (eql q-result result)
			      (eql q-result-conj result-conj)
			      (state-array-equal (schema-context-array schema) item-array))	; The slow case.
		     (return-from fake-conj-find-plus i)))))))
  nil)						; No match.

;;; ---

;;; +++ Finally, we can get to the main function here.

;;; Like the old INSIDE-OUT-MAYBE-SPINOFF-SCHEMA (now punted), but consults two
;;; global variables to figure out which generators to use for schemas and items.
;;; Also, this version updates several counters, for proof-of-concept benchmarking.
(defun RECONFIGURABLE-INSIDE-OUT-MAYBE-SPINOFF-SCHEMA ()
  (with-spinoff-utilities
    (with-metered-spinoffs
      (with-context-spinoff-markers
	(with-iterator (yield-schema-index-fn reset-schema-index-fn *use-which-spinoff-schema-number-generator?*)
	  (with-iterator (yield-item-index-fn reset-item-index-fn *use-which-spinoff-item-number-generator?*)
	    (do-iterator (yield-item-index-fn reset-item-index-fn item-index)
	      ;; These walk down the extended-contexts or -results of schemas we investigate in sync with the item number.
	      ;; Since the item numbers aren't sequential, we have to recompute these for each new item.
	      (let ((array-index (get-counter-array-index item-index))
		    (record-offset (counter-record-offset (get-counter-record-position item-index))))
		(do-iterator (yield-schema-index-fn reset-schema-index-fn schema-index)
		  (incf .inner-loop-counter.)
		  (let ((schema (get-schema schema-index)))
		    (cond ((schema-result-empty-p schema)
			   ;; Empty result, so see if we can make some spinoff result schemas.
			   ;; [E.g., this must be a BARE schema, which are rare in the population, though spinoffs from them are common,
			   ;; especially early in the run.]
			   (let ((result-pos        (schema-extended-result-pos schema))
				 (result-neg        (schema-extended-result-neg schema))
				 (result-pos-conj   (schema-extended-result-conj-pos schema))
				 (result-children   (schema-result-children schema))
				 (res-conj-children (schema-result-conj-children schema)))
			     (when (item-not-in-state-children? result-children item-index)
			       ;; If this schema hasn't spun off a (result) child that also looks at this item, check reliabilities to see if we want to now.
			       (when (counter-pinned-and-positive? result-pos)	; Reliable.
				 (metered-make-spinoff-result schema-index item-index (make-state-on)))
			       (when (counter-pinned-and-positive? result-neg)	; Reliable.
				 (metered-make-spinoff-result schema-index item-index (make-state-off))))
			     (when (and (fix< item-index *conj-number*)	; $OPT:  Combine these WHENs?
					(item-not-in-flag-children? res-conj-children item-index)
					(counter-pinned-and-positive? result-pos-conj))	; Reliable.
			       ;; This schema hasn't spun off a (result-conj) child that also looks at this item, and its counter is pinned:  spin it off.
			       (metered-make-spinoff-result-conj schema-index item-index))))
			  (t
			   ;; Nonempty result, so see if we can make some spinoff context schemas.
			   ;; [E.g., this is a RESULT schema, which are common in the population, though spinoffs from them can't happen until results happen.]
			   (let* ((ext-context      (schema-extended-context schema))
				  (context-children (schema-context-children schema))
				  (new-item-state   (if (counter-positive? ext-context)
							(make-state-on)
							(make-state-off))))
			     (when (and (item-not-in-state-children? context-children item-index)
					(counter-pinned? ext-context)	; Reliable.
					(zerop (bit context-spinoff-markers schema-index))
					(not (schema-plus-item-already-a-conj? schema item-index new-item-state))
					(not (fake-conj-find-plus schema item-index new-item-state)))
			       ;; This schema hasn't spun off a (context) child that also looks at this item, nor have we made a more-specific one:  spin off.
			       (setf (bit context-spinoff-markers schema-index) 1)	; Don't (later) make a less-specific one for this schema.
			       (metered-make-spinoff-context schema-index item-index new-item-state)))))))))))))))

;;; These all use ' instead of #' so the relevant variables get bound to symbols, not
;;; compiled-function objects.  This allows us to portably print the names of
;;; whatever functions they're bound to, and, in particular, to portably use those
;;; names to build new function names (e.g., in the composite functions created in
;;; GOALS.LISP).  We have to do this because, while (FORMAT T "~A" #'FOO) prints
;;; FOO in Genera (though ~S would print #<Compiled function FOO>), it prints
;;; #<Function FOO> in LispWorks (for example).

(defmacro WITH-SPINOFF-FULL-CROSSBAR-GENERATORS (&body body)
  `(with-spinoff-schema-number-generator 'all-schema-numbers-generator
     (with-spinoff-item-number-generator 'all-item-numbers-generator
       ,@body)))

(defmacro WITH-SPINOFF-CHANGED-ITEMS-ALL-SCHEMAS-GENERATORS (&body body)
  `(with-spinoff-schema-number-generator 'all-schema-numbers-generator
     (with-spinoff-item-number-generator 'changed-item-numbers-most-specific-first-generator
       ,@body)))

(defmacro WITH-SPINOFF-ATTENTIVE-TO-PRIOR-TICK-GENERATORS (&body body)
  `(with-spinoff-schema-number-generator 'mod-all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
     (with-spinoff-item-number-generator 'changed-item-numbers-most-specific-first-generator
       ,@body)))

(defmacro WITH-SPINOFF-SCHEMAS-WITH-RECENTLY-UPDATED-STATS-GENERATORS (&body body)
  `(with-spinoff-schema-number-generator 'schemas-with-recently-updated-stats-generator
     (with-spinoff-item-number-generator 'changed-item-numbers-most-specific-first-generator
       ,@body)))

;;;; Limited focus-of-attention in statistical updating.

(def-iterator-generator UEIS
  all-schema-numbers-generator
  all-item-numbers-generator)

(def-vector-metering-counter-periodically-reported *UEIS-INNER-LOOP-COUNTERS*
						   "~:D total stat inner loop iteration~:P.")
(def-vector-metering-counter *UEIS-MODIFY-WHEN-POSITIVE-COUNTERS*)
(def-vector-metering-counter *UEIS-MODIFY-WHEN-NEGATIVE-COUNTERS*)
(def-vector-metering-counter *UEIS-MODIFY-WHEN-CONTEXT-COUNTERS*)

(defmacro WITH-METERED-UEIS (&body body)
  `(let ((.inner-loop-counter.   0)
	 (.modify-when-positive. 0)
	 (.modify-when-negative. 0)
	 (.modify-when-context.  0)
	 (.last-unique-schema-modified-positive. -1)
	 (.last-unique-schema-modified-negative. -1)
	 (.last-unique-schema-modified-context.  -1)
	 (.unique-schemas-modified-positive. 0)
	 (.unique-schemas-modified-negative. 0)
	 (.unique-schemas-modified-context.  0))
     (prog1
       ,@body
       (push .inner-loop-counter. *ueis-inner-loop-counters*)
       (push .modify-when-positive. *ueis-modify-when-positive-counters*)
       (push .modify-when-negative. *ueis-modify-when-negative-counters*)
       (push .modify-when-context.  *ueis-modify-when-context-counters*)
       (push .unique-schemas-modified-positive. *unique-schema-stats-modified-each-iteration-positive*)
       (push .unique-schemas-modified-negative. *unique-schema-stats-modified-each-iteration-negative*)
       (push .unique-schemas-modified-context.  *unique-schema-stats-modified-each-iteration-context*)
       )))
     
;;; I dunno, this _might_ be useful elsewhere, and I have to put it outside WITH-UEIS-UTILITIES
;;; to get the indentation to work, unless I make a dummy external macro to do that or something...
(defmacro WITH-ACCESSORS-FROM-ITEM-INDEX ((item-index array-index record-offset) &body body)
  (let ((record-position (gensym)))
    `(multiple-value-bind (,array-index ,record-position)
	 (get-counter-array-index ,item-index)
       (let ((,record-offset (counter-record-offset ,record-position)))
	 ,@body))))

;;; Warning:  There are free references all over the place in this to variables used
;;; inside various scopes of NEW-SCHEMA-UPDATE-EXT-ITEM-STATS.  It must also be
;;; called inside WITH-METERED-UEIS.
(defmacro WITH-UEIS-UTILITIES (&body body)
  `(macrolet ((some-transition? (old-state-was state-array)
		`(and (,old-state-was last-state)
		      (not (and (flag-falsep activated)	; If it wasn't activated, ...
				(predicted-result item-index)))	; ... but some other schema predicted this result, we don't want it.
		      (flag-eq (counter-array-toggle ,state-array array-index record-offset)
			       activated)))
	      (positive-transition? ()
		`(some-transition? state-off-p positive))
	      (negative-transition? ()
		`(some-transition? state-on-p  negative))
	      (show-counter-state (format-string state-array)
		`(ext-stats-format ,format-string (counter-unparse-from-array ,state-array array-index record-offset)))
	      (with-toggled-counter (state-array &body body)
		`(progn
		   (show-counter-state "~A->" ,state-array)
		   (counter-array-toggle-toggle ,state-array array-index record-offset)
		   ,@body
		   (show-counter-state "~A" ,state-array)))
	      (update-schema-tick-vector (type)
		(let ((vector-symbol (intern-format "*schema-tick-vector-~A*" type)))
		  `(setf (aref ,vector-symbol schema-index) *clock-tick*)))
	      (update-unique-schemas-modified (type)
		(let ((last-symbol (intern-format ".last-unique-schema-modified-~A." type))
		      (uniq-symbol (intern-format ".unique-schemas-modified-~A." type)))
		  `(unless (= ,last-symbol schema-index)
		     (incf ,uniq-symbol)
		     (setf ,last-symbol schema-index))))
	      (modify-when (state-sensor state-array occurred type)
		(let ((meter-symbol (intern-format ".modify-when-~A." type)))
		  `(cond (,state-sensor
			  (incf ,meter-symbol)
			  (update-schema-tick-vector ,type)
			  (update-unique-schemas-modified ,type)
			  (with-toggled-counter ,state-array (counter-array-modify-value ,state-array array-index record-offset ,occurred)))
			 (t
			  (with-toggled-counter ,state-array)))))
	      (result-empty-update ()
		`(cond ((positive-transition?)
			(ext-stats-format "+t ")
			(modify-when (state-on-p  current-state) positive activated positive))
		       ((negative-transition?)
			(ext-stats-format "-t ")
			(modify-when (state-off-p current-state) negative activated negative)))))
     ,@body))

(defun NEW-SCHEMA-UPDATE-EXT-ITEM-STATS ()
  (with-metered-ueis
    (with-ueis-utilities
      (with-iterator (yield-schema-index-fn reset-schema-index-fn *use-which-ueis-schema-number-generator?*)
	(with-iterator (yield-item-index-fn reset-item-index-fn *use-which-ueis-item-number-generator?*)
	  (do-iterator (yield-item-index-fn reset-item-index-fn item-index)
	    (let ((current-item (get-item item-index)))
	      (with-accessors-from-item-index (item-index array-index record-offset)
		(ext-stats-format "~&Iteration ~D, item ~D:~&" *clock-tick* item-index)	; DBG.
		(item-format "~A~%" current-item)
		(do-iterator (yield-schema-index-fn reset-schema-index-fn schema-index)
		  (incf .inner-loop-counter.)
		  (let ((schema (get-schema schema-index)))
		    (cond ((schema-result-empty-p schema)	; HEY!  This can ONLY happen for BARE schemas!  Hmm!
			   (let ((activated     (schema-activated schema))
				 (positive      (schema-extended-result-pos schema))
				 (negative      (schema-extended-result-neg schema))
				 (current-state (item-current-state current-item))
				 (last-state    (item-last-state current-item)))
			     (ext-stats-format "~4D res " schema-index)
			     (result-empty-update)))	; A lot of work happens here...
			  (t
			   ;; non-empty result so update extended-context
			   (ext-stats-format "~4D con " schema-index)
			   (cond ((and (schema-activated-p schema)
				       (or (state-unknown-p (state-array-get-state (schema-context-children schema) item-index))
					   (state-noteq     (state-array-get-state (schema-context-children schema) item-index)
							    (item-last-state current-item))))
				  (let* ((ext-context (schema-extended-context schema))
					 (last-state  (item-last-state current-item))
					 (toggle      (counter-array-toggle ext-context array-index record-offset)))
				    (cond ((or (and (flag-truep  toggle) (state-on-p  last-state))
					       (and (flag-falsep toggle) (state-off-p last-state)))
					   (ext-stats-if (state-on-p last-state)
							 (ext-stats-format "+t ")
							 (ext-stats-format "-t "))
					   (modify-when (schema-result-satisfied-p schema)
							ext-context
							(flag-parse (state-on-p last-state))
							context))
					  (t
					   (ext-stats-format "no match state/toggle")))))
				 (t
				  (ext-stats-format "not activated or deferred"))))))
		  (schema-update-framework-iffer ext-stats)))
	      (ext-stats-format "~%"))))))))

;;; See comments above (in the -SPINOFF- analog to this stuff) about why we're using ' and not #' here.

;;; Hmm.  I just noticed (1 Sep 93) that the -most-specific-first part of this wasn't necessary, since
;;; the statistical stuff (unlike the spinoff stuff) doesn't care what order the items come in.  No semantic
;;; change, but a slight wallclock runtime change, I assume.
(defmacro WITH-UEIS-ATTENTIVE-TO-PRIOR-TICK-GENERATORS (&body body)
  `(with-ueis-schema-number-generator 'mod-all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
     (with-ueis-item-number-generator 'changed-item-numbers-most-specific-first-generator
       ,@body)))

(defmacro WITH-UEIS-CHANGED-ITEMS-ALL-SCHEMAS-GENERATORS (&body body)
  `(with-ueis-schema-number-generator 'all-schema-numbers-generator
     (with-ueis-item-number-generator 'changed-item-numbers-generator
       ,@body)))

(defmacro WITH-UEIS-ALL-ITEMS-DEPENDENT-SCHEMAS-GENERATORS (&body body)
  `(with-ueis-schema-number-generator 'mod-all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
     (with-ueis-item-number-generator 'all-item-numbers-generator
       ,@body)))

(defmacro WITH-UEIS-HISTORY-ITEMS-ALL-SCHEMAS-GENERATORS (&body body)
  `(with-ueis-schema-number-generator 'all-schema-numbers-generator
     (with-ueis-item-number-generator 'changed-item-numbers-in-history-generator
       ,@body)))

(defmacro WITH-UEIS-HISTORY-ITEMS-DEPENDENT-SCHEMAS-GENERATORS (&body body)
  `(with-ueis-schema-number-generator 'all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
     (with-ueis-item-number-generator 'changed-item-numbers-in-history-generator
       ,@body)))

;;; Amazingly enough, this one didn't exist until 24 Apr 94.  I guess that I created it
;;; "by hand" when doing the complete statistical runs (the ones that bound each of
;;; the four filter individually), but then never actually created what I deemed to be
;;; the "right" one of these for later use until now.
(defmacro WITH-UEIS-CHANGED-ITEMS-DEPENDENT-SCHEMAS-GENERATORS (&body body)
  `(with-ueis-schema-number-generator 'all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
     (with-ueis-item-number-generator 'changed-item-numbers-in-history-generator
       ,@body)))

;;; End of file.
