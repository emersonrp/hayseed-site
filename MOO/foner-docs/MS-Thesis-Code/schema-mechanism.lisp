;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; The running routines that make up the schema mechanism.
;;;; This was originally the last 40% of SCHEMA.LISP.

;;; [This file was inspired by Ramstad's implementation of Drescher's
;;; system, but Ramstad's version has by now been 80% or more rewritten.]

(in-package :schema)

;;; The reason that these are expressed as two separate macros is because the
;;; structure of some of the updaters (currently, SCHEMA-UPDATE-EXT-ITEM-STATS,
;;; SCHEMA-UPDATE-EXT-CONJ-STATS, and NEW-SCHEMA-UPDATE-EXT-ITEM-STATS)
;;; don't quite fit the model assumed by WITH-UPDATE-FRAMEWORK.  Hence, we break
;;; things out so I can still use some of the modularization below.  The reason for the
;;; little IFFER macros is because NEW-SCHEMA-UPDATE-EXT-ITEM-STATS basically can't
;;; take advantage of any of this stuff anyway, because it's based on iterators.
(defmacro UPDATE-FRAMEWORK-IFFER (what iteration-variable)
  (let ((iffer (intern-format "~A-if" what)))
    `(,iffer (fix= 0 (rem ,iteration-variable 2))
      (safe-format *output-stream* "~35T")	; !SF!
      (format *output-stream* "~%"))))

(defmacro WITH-UPDATE-FRAMEWORK-INNARDS (what
					 (iteration-variable iteration-limit)
					 (individual-variable individual-accessor)
					 (format-string &rest format-args)
					 &body body)
  (let ((formatter (intern-format "~A-format" what)))
    ;; The original code, in most cases, used DOTIMES instead of LOOP.
    ;; However, I had to change to LOOP in the case of SCHEMA-UPDATE-EXT-CONJ-STATS
    ;; for what I can only think is a Genera compiler bug, so I'll use LOOP for all the rest, too.
    `(loop for ,iteration-variable from 0 below ,iteration-limit
	   do (let ((,individual-variable (,individual-accessor ,iteration-variable)))
		,@body
		(when ,format-string  ; Allow turning it off.  (It'd be cleaner to do this at compile-time, not runtime, but this is easy & not too slow.)
		  (,formatter ,format-string ,@format-args)))
	      (update-framework-iffer ,what ,iteration-variable))))

(defmacro WITH-UPDATE-FRAMEWORK ((what pretty-what)
				 (iteration-variable iteration-limit)
				 (individual-variable individual-accessor)
				 (format-string &rest format-args)
				 &body body)
  (let ((formatter (intern-format "~A-format" what)))
    `(progn
       (,formatter (format nil "updating ~A~%" ,pretty-what))
       (with-update-framework-innards ,what
				      (,iteration-variable ,iteration-limit)
				      (,individual-variable ,individual-accessor)
				      (,format-string ,@format-args)

	 ,@body)
       (,formatter "~%"))))

;;; It turns out that all the callers of the above three macros, now that they're properly broken out,
;;; always do so when they're mapping over schema (not items, conjs, or whatever), and they always
;;; name their iteration variables the same, too.  So package up this common idiom and make life easier.
(defmacro WITH-SCHEMA-UPDATE-FRAMEWORK-INNARDS (what
						(format-string &rest format-args)
						&body body)

  `(with-update-framework-innards ,what
				  (schema-index *schema-number*)
				  (schema get-schema)
				  (,format-string ,@format-args)
     ,@body))

(defmacro WITH-SCHEMA-UPDATE-FRAMEWORK (what
					(format-string &rest format-args)
					&body body)
  `(with-update-framework ,what
			  (schema-index *schema-number*)
			  (schema get-schema)
			  (,format-string ,@format-args)
     ,@body))
  
(defmacro SCHEMA-UPDATE-FRAMEWORK-IFFER (what)
  `(update-framework-iffer ,what schema-index))

;;;; Major function:  SCHEMA-UPDATE-APPLICABLE.

(defmacro CONTEXT-SATISFIED-P (array)
  `(state-array-included-p *microworld-state* ,array *microworld-state-size*))

(defmacro CONJ-SATISFIED-P (conj-index)
  `(state-on-p (conj-current-state
                (get-conj ,conj-index))))

;;; For each schema with satisfied context, mark it as applicable.
;;; An empty context is always satisfied, otherwise, it is satisfied if
;;; a) the context has been made into a conj and the conj is ON
;;; b) each state in the context is matched by the microworld state.

;;; Broken out so I can debug some applicability probems with goals.
(defsubst COMPUTE-SCHEMA-APPLICABLE (schema)
  (or (schema-context-empty-p schema)
      (if (schema-context-conj-p schema)
	  (conj-satisfied-p
	    (schema-context-item schema))
	  (context-satisfied-p
	    (schema-context-array schema)))))

(defun SCHEMA-UPDATE-APPLICABLE ()
  (with-schema-update-framework (applicable "applicable")
				("~4D ~25A~A"
				 schema-index
				 (schema-print-name schema)
				 (flag-unparse (schema-applicable schema)))
    (setf (schema-applicable schema)
	  (flag-parse
	    (compute-schema-applicable schema)))))

;;; *** start update accessibility / create goal-directed actions ***

;;; NOTE:  The MIT Press version of Drescher's algorithm doesn't use
;;; any of this code, rather, goal-directed actions are created for
;;; each unique result which is included in a schema.

;;; This code updates accessibility for items and conjunctions and
;;; makes goal-directed-actions for those which are highly accessible.
;;; Must be called *after* schemas have been marked applicable.

;;; The following two functions (ACCESSIBLE-KEY and ACCESSIBLE-TEST) are
;;; used via ASSOC by the main UPDATE-ACCESSIBILITY routine -- they
;;; should only be used by that routine (they are fairly specifically
;;; designed for that routine only).

;;; ACCESSIBLE-TEST assumes that RESULT-SCHEMA has a non-NIL result
;;; (obviously not interesting), and that CONTEXT-SCHEMA has a non-NIL
;;; context (as the first pass in the main UPDATE-ACCESSIBILITY insures
;;; that these are all dealt with).

;;; When calling via ASSOC, RESULT-SCHEMA is bound to the result of
;;; evaluating the key for the given car of the ASSOC pair 
;;; (in the case of the UPDATE-ACCESSIBILITY routine, the CAR is a
;;; schema-index, and the ACCESSIBLE-KEY function gives us the actual
;;; schema).
;;; CONTEXT-SCHEMA is bound to the argument that ASSOC is looking for
;;; (in the UPDATE-ACCESSIBILITY routine, it is the schema for which we
;;; are attempting to find a result which chains to its context).
;;; Returns T when result of RESULT-SCHEMA implies context of
;;;   CONTEXT-SCHEMA.

(defun ACCESSIBLE-KEY (y)
  (get-schema y))

(defun ACCESSIBLE-TEST (context-schema result-schema)
  ;; RESULT must be non-NIL to satisfy anything.
  (let ((result-data (schema-data result-schema))
        (context-data (schema-data context-schema)))
    (if (schema-data-result-conj-p result-data)
        ;; If result conjunction, check to see if context
        ;; is conjunction too.
        (if (schema-data-context-conj-p context-data)
            ;; If so, entry in the result conjunction inclusion
            ;; array set correctly implies context satisfied.
            (flag-truep
	      (flag-array-get-flag
		(conj-inclusion-array
		  (get-conj (schema-result-item result-schema)))
		(schema-context-item context-schema)))
	    ;; If context not conjunction, check array directly.
	    (state-array-included-p
	      (conj-item-array
		(get-conj (schema-result-item result-schema)))
	      (schema-context-array context-schema)
	      *fixna-required-to-hold-all-item-states*))
	;; Otherwise, context must be single, and stuff match up
	;; (return the value t or nil as appropriate).
	(and (schema-data-context-single-p context-data)
	     (state-eq (if (schema-data-result-negated-p result-data)
			   (make-state-off)
			   (make-state-on))
		       (state-array-get-state
			 (schema-context-array context-schema)
			 (schema-result-item result-schema)))))))

;;; To speed up computation in ASSOC.
;;; &&& ---> I'll bet that making this just expand into > for Genera (and eliminating a function call!)
;;; would be a major win.  Try this out at some point.  E.g., #+Genera (deff 'truefloat> #'>) or something...
;;; [Gee, I take it back---#'> probably runs slower, given its disassembly---it has to handle a &rest arg,
;;; whereas the > in truefloat> is a single instruction, because the compiler can tell how many args it's getting...]
(defun TRUEFLOAT> (x y)
  (> (the short-float x)
     (the short-float y)))

(defvar *ACCESSIBLE-ITEM-POS*
	(make-flag-array *fixna-required-to-hold-all-item-flags*))

(defvar *ACCESSIBLE-ITEM-NEG*
	(make-flag-array *fixna-required-to-hold-all-item-flags*))

(defvar *ACCESSIBLE-CONJ-POS*
	(make-flag-array *fixna-required-to-hold-all-conj-flags*))

;;; Description of UPDATE-ACCESSIBILITY algorithm:
;;; When a schema is reached through any path, mark it.
;;; For the first round, form a list of all applicable schemas with
;;; medium reliability and non-nil results.
;;; For every round, keep a list of schemas reached on the previous
;;; round, with corresponding reliability, sorted from highest to
;;; lowest.
;;; To form the next round of schemas, go through all schemas,
;;; using FIND to see if any members of the last round list
;;; have a result which chains to the context of the current schema.
;;; If a schema is found which can be chained to,
;;; if marked as visited previously, and reliability higher, then
;;; update old value and put on next round list -- otherwise ignore.
;;; If not visited previously and above threshold, put on next round
;;; list .
;;; In this fashion, every reachable schema through a path of
;;; accessible schemas can be found and marked.
;;; The result item or conjunction for each marked schema is then
;;; marked -- and then each item and conjunction has its accessibility
;;; updated.

;;; Note:  current behavior of program has conjunctive result ->
;;; conjunctive context -- but if a conjunction *includes* another one,
;;; that conjunction isn't marked explicitly accessible (unless it is
;;; accessible through some other path).

;;; Accessibility in the negative direction for conjunctions is
;;; definitely not important -- a negated conjunction is a disjunction,
;;; and a disjunctive goal makes no sense within the context of this
;;; mechanism.

;;; To avoid unnecessary proliferation of fairly useless goal-directed
;;; actions, this routine was modified to as to not have negated items
;;; as goals either -- this departure from the original paper can be
;;; undone by carefully removing commented out code below and in the
;;; definition of the item datatype.

;;; Major function:  UPDATE-ACCESSIBILITY.

(defparameter *UPDATE-ACCESSIBILITY-NUMBER-OF-PASSES* 5)	; [This was originally part of a hardcoded (DOTIMES (Y 4) ...) sort of thing.]

(defun UPDATE-ACCESSIBILITY ()
  (flag-array-clear *accessible-item-pos* *fixna-required-to-hold-all-item-flags*)
  #+gd-actions-negative
  (flag-array-clear *accessible-item-neg* *fixna-required-to-hold-all-item-flags*)
  (flag-array-clear *accessible-conj-pos* *fixna-required-to-hold-all-conj-flags*)
  (let ((visited nil)
        (old-visited nil))
    (dotimes (x *schema-number*)
      (let* ((schema (get-schema x))
	     (data (schema-data schema)))
	(cond ((and (schema-data-applicable-p data)
		    (flag-falsep (schema-data-result-empty data))
		    (weighted-rate-medium-p (schema-reliability schema)))
	       (setf (schema-marked schema) (make-flag-true))
	       (setq visited (acons x (schema-reliability schema) visited)))
	      (t
	       (setf (schema-marked schema) (make-flag-false))))))
    (setq visited (sort visited  #'truefloat> :key #'cdr))
    (setq old-visited (copy-alist visited))
    (accessibility-format "initial visited ~A~%" visited)
    (dotimes (y (1- *update-accessibility-number-of-passes*))
      (let ((new-visited nil))
	y					; In case ACCESSIBILITY-FORMAT is compiled not to do anything.
	(accessibility-format "pass ~D~%" y)
	(accessibility-format "old-visited ~A~%" old-visited)
	(dotimes (x *schema-number*)
	  (let* ((schema (get-schema x))
		 (data (schema-data schema))
		 (reliability (schema-reliability schema)))
	    (declare (short-float reliability))
	    ;; If empty result, not interesting, and must also have high
	    ;; enough reliability (otherwise anything found returned
	    ;; would be too low to have new-reliability above the threshold).
	    (when (and (flag-falsep (schema-data-result-empty data))
		       (weighted-rate-medium-p reliability))
	      ;; The obscure [in Ramsta'd opinion, anyway--- Foner] ASSOC
	      ;; command sends the result of applying ACCESSIBLE-KEY to
	      ;; each element of OLD-VISITED, along with SCHEMA, to
	      ;; ACCESSIBLE-TEST -- ACCESSIBLE-TEST is expected to
	      ;; return T if one is found where the schema in question
	      ;; has a result which includes the context of SCHEMA.
	      (let* ((found (assoc schema old-visited
				   :key #'accessible-key
				   :test #'accessible-test)))
		(when found
		  (let ((new-reliability
			  (weighted-rate* reliability (cdr found))))
		    (declare (short-float new-reliability))
		    (accessibility-format
		      "schema ~D reliability ~6,4F prior-reliability ~6,4F ~
                        new-reliability ~6,4F~%"
		      x reliability (cdr found) new-reliability)
		    ;; if already marked
		    (if (schema-marked-p schema)
			;; Check to see if this is a more reliable path.
			(when (float> new-reliability (cdr (assoc x visited)))
			  ;; If so, replace the old value with the new
			  ;; one, and add it to the new-visited array
			  ;; (as the new value may allow certain
			  ;; children which didn't succeed before to
			  ;; succeed this time).
			  ;; &&& Can I store the assoc value instead of recomputing?  Or would I need a locative?
			  (rplacd (assoc x visited) new-reliability)
			  (setq new-visited
				(acons x new-reliability new-visited)))
			;; Otherwise not marked, so mark and add to
			;; new-visited array if greater than threshold.
			(when (weighted-rate-medium-p new-reliability)
			  (setf (schema-marked schema) (make-flag-true))
			  (setq new-visited
				(acons x new-reliability new-visited)
				visited
				(acons x new-reliability visited))))))))))
	(unless new-visited
	  (return))
	;; At this point, NEW-VISITED has what should be used for
	;; old-visited on the next iteration.
	(setq new-visited (sort new-visited #'truefloat> :key #'cdr))
	(setq old-visited new-visited)
	(accessibility-format "visited ~A~%" visited))))
  ;; At this point, all "accessible" schemas should be marked, so go
  ;; through them, if result conjunctive, mark the included
  ;; conjunctions (which includes the result conjunction).
  ;; If not, just mark the item(s).
  (dotimes (x *schema-number*)
    (let* ((schema (get-schema x))
	   (data (schema-data schema)))
      (when (and (schema-data-marked-p data)
		 (flag-falsep (schema-data-result-negated data)))
	(let ((result-item (schema-result-item schema)))
	  (if (schema-data-result-conj-p data)
	      (flag-array-ior (conj-inclusion-array
				(get-conj result-item))
			      *accessible-conj-pos*
			      *fixna-required-to-hold-all-conj-flags*)
	      (setf (flag-array-get-flag
		      *accessible-item-pos*
		      result-item)
		    (make-flag-true)))))))
  ;; Old version for negated results (note: doesn't do inclusive
  ;; conjunctions right).
  #+gd-actions-negative
  (dotimes (x *schema-number*)
    (let* ((schema (get-schema x))
	   (data (schema-data schema)))
      (when (schema-data-marked-p data)
	(let ((result-item (schema-result-item schema)))
	  (if (schema-data-result-conj-p data)
	      (if (flag-falsep (schema-data-result-negated data))
		  (setf (flag-array-get-flag
			  *accessible-conj-pos*
			  result-item)
			(make-flag-true)))
	      (setf (flag-array-get-flag
		      (if (flag-truep (schema-result-negated schema))
			  *accessible-item-neg*
			  *accessible-item-pos*)
		      result-item)
		    (make-flag-true)))))))
  ;; Actually modify the accessibility numbers for the items and conjunctions.
  ;; For each marked conjunction, increase accessibility and mark the
  ;; included (positive) items.
  (accessibility-format "updating accessibility~%")
  ;; In the usual (original) case, *FIXNA-REQUIRED-TO-HOLD-ALL-ITEM-FLAGS*
  ;; (and hence the length of *RELIABLE-ITEM-POS*) is smaller than
  ;; *FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-FLAGS* (and hence the length of
  ;; (CONJ-xxx-FLAG-ARRAY ...)).  However, in the case where I've boosted
  ;; the maximum number of conj's from 300 to 3000, this is no longer the case.
  ;; [This reasoning stolen from SYN-ITEM-UPDATE-PHASE-THREE.]
  (let ((minimal-length (min *fixna-required-to-hold-all-conj-flags*
			     *fixna-required-to-hold-all-item-flags*)))
    (dotimes (x *conj-number*)
      (let ((conj (get-conj x)))
	(cond ((flag-truep (flag-array-get-flag *accessible-conj-pos* x))
	       (conj-acc-pos-update conj t)
	       (flag-array-ior (conj-pos-flag-array conj)
			       *accessible-item-pos*
;                              *fixna-required-to-hold-all-conj-flags*)	; CONJ, not ITEM, because the first arg to the IOR is only CONJ long.
			       minimal-length)
	       (accessibility-format "~A~%" conj)
	       (when (and (flag-falsep (conj-gd-pos-created conj))
			  (conj-acc-pos-high-p conj))
		 (setf (conj-gd-pos-created conj) (make-flag-true))
		 (make-gd-action-conj-pos x)))
	      (t
	       (conj-acc-pos-update conj nil)
	       (accessibility-format "~A~%" conj))))))
  (dotimes (x *item-number*)
    (let ((item (get-item x)))
      (item-acc-pos-update
	item
	(flag-truep (flag-array-get-flag *accessible-item-pos* x)))
      #+gd-actions-negative
      (item-acc-neg-update
	item
	(flag-truep (flag-array-get-flag *accessible-item-neg* x)))
      (accessibility-format "~A~%" item)
      (when (and (flag-falsep (item-gd-pos-created item))
		 (item-acc-pos-high-p item))
	(setf (item-gd-pos-created item) (make-flag-true))
	(make-gd-action-item-pos x))
      #+gd-actions-negative
      (when (and (flag-falsep (item-gd-neg-created item))
		 (item-acc-neg-high-p item))
	(setf (item-gd-neg-created item) (make-flag-true))
	(make-gd-action-item-neg x))
      )))

;;;; Create goal-directed action functions.

(defun MAKE-GD-ACTION-ITEM-POS (item-index)
  (main-format "~5D goal-directed-action-item-pos-created ~D ~A~%"
               *clock-tick*
               item-index
               (item-print-name (get-item item-index))))

#+gd-actions-negative
(defun MAKE-GD-ACTION-ITEM-NEG (item-index)
  (main-format "~5D goal-directed-action-item-neg-created ~D ~A~%"
	       *clock-tick*
	       item-index
	       (item-print-name (get-item item-index))))

(defun MAKE-GD-ACTION-CONJ-POS (conj-index)
  (main-format "~5D goal-directed-action-conj-pos-created ~D ~A~%"
               *clock-tick*
               conj-index
               (conj-print-name (get-conj conj-index))))

;;; *** end update accessibility / create goal-directed actions code ***

;;;; Major functions:  ITEM-UPDATE-STATE and CONJ-UPDATE-STATE.

(defun ITEM-UPDATE-STATE ()
  (dotimes (x *item-number*)
    (let ((current-item (get-item x)))
      (if (item-syn-item-p current-item)
	  (setf
	    ;; Last state (post) = current state (pre).
	    (item-last-state current-item) (item-current-state current-item)
	    ;; Put "both" into current-state and microworld-state.
	    ;; "Both" is 11 and inclusive ORs with anything.
	    ;; Written into each synthetic item state while determining
	    ;; the state of the primitive items.
	    (item-current-state current-item) (make-state-both)
	    (get-microworld-state x) (make-state-both))
	  (let ((new-state (state-parse (funcall (item-code current-item)))))
	    (setf
	      ;; Last state (post) = current state (pre).
	      (item-last-state current-item) (item-current-state current-item)
	      ;; Current state (post) = result of calling code.
	      (item-current-state current-item) new-state
	      (get-microworld-state x) new-state)
	    ;; Update generality -- increment rate if state is ON.
	    (item-generality-update current-item (state-on-p new-state)))))))

(defun CONJ-UPDATE-STATE ()
  (dotimes (x *conj-number*)
    (let* ((current-conj (get-conj x))
	   (new-state (state-parse
			(context-satisfied-p
			  (conj-item-array current-conj)))))
      (setf
	;; Last state (post) = current state (pre).
	(conj-last-state current-conj) (conj-current-state current-conj)
	;; Current state (post) = result of calling code.
	(conj-current-state current-conj) new-state))))

;;;; Major function:  SCHEMA-UPDATE-ACTIVATED.

;;; If schema has same action, is not a goal-directed-action schema,
;;; and is applicable (i.e. context-satisfied) then is activated.
;;; Otherwise, if goal-directed-action and result satisfied, mark activated.

(defun SCHEMA-UPDATE-ACTIVATED (action-index)
  (with-schema-update-framework (activated "activated")
				("~4D ~25A~A"
				 schema-index
				 (schema-print-name schema)
				 (flag-unparse (schema-activated schema)))
    (setf (schema-activated schema)
	  (flag-parse
	    (and (fix= (schema-action-item schema) action-index)
		 (if (schema-action-gd-p schema)
		     (schema-result-satisfied-p schema)
		     (schema-applicable-p schema)))))))

;;;; Major function:  SYN-ITEM-UPDATE-STATE.

(defvar *RELIABLE-ITEM-POS* (make-flag-array *fixna-required-to-hold-all-item-flags*))
(defvar *RELIABLE-ITEM-NEG* (make-flag-array *fixna-required-to-hold-all-item-flags*))
(defvar *RELIABLE-CONJ*     (make-flag-array *fixna-required-to-hold-all-conj-flags*))

(defun SCHEMA-NOT-CONTEXT-OVERRIDDEN-P (schema)
  (let ((record-offset 0)
        (array-index 0)
        (ext-context (schema-extended-context schema)))
    ;; Iterate through all items.
    (dotimes (x *item-number* t)
      ;; For each, check to see if the counter value is 13 or higher.
      (if (fix< 12
		(counter-array-value
		  ext-context array-index record-offset))
	  ;; If so, check to see if counter positive and item OFF
	  ;; or counter negative and item ON.
	  ;; If either is true, overridden.
	  (if (flag-truep
		(counter-array-pos
		  ext-context array-index record-offset))
	      (if (state-off-p (get-microworld-state x))
		  (return nil))
	      (if (state-on-p (get-microworld-state x))
		  (return nil))))
      (if (fix= *counter-record-max-offset* record-offset)
	  (setq record-offset 0
		array-index (fix1+ array-index))
	  (setq record-offset (fix+ *counter-bits* record-offset))))))

;;; When the current state is KNOWN, put in current-state.
;;; When it is just guessed, put in maybe-state.
;;; SET-TIME is updated when current-state is changed, this can easily
;;; be used to check and make sure an earlier (higher precedent) value
;;; isn't being clobbered.

(defun SYN-ITEM-UPDATE-STATE ()
  (dotimes (x *syn-item-number*)
    (setf (syn-item-maybe-state (get-syn-item x))
	  (make-state-unknown)))
  (schema-update-host-results)
  (syn-item-update-phase-one)
  (syn-item-update-phase-two-a)
  (syn-item-update-phase-three)
  (syn-item-update-phase-two-b)
  (syn-item-update-phase-four)
  (syn-item-update-phase-five))

(defun SCHEMA-UPDATE-HOST-RESULTS ()
  (with-schema-update-framework (result "host schema results")
				("~4D ~25A~A"
				 schema-index
				 (schema-print-name schema)
				 (flag-unparse (schema-result-satisfied schema)))
    (when (schema-syn-item-p schema)
      (setf (schema-result-satisfied schema)
	    (flag-parse
	      (or (schema-result-empty-p schema)
		  (if (schema-result-conj-p schema)
		      (conj-satisfied-p
			(schema-result-item schema))
		      (or (state-eq
			    (if (schema-result-negated-p schema)
				(make-state-off)
				(make-state-on))
			    (get-microworld-state
			      (schema-result-item schema)))
			  (state-eq
			    (make-state-both)
			    (get-microworld-state
			      (schema-result-item schema)))))))))))

(defun SYN-ITEM-UPDATE-PHASE-ONE ()
  ;; PHASE ONE
  ;; If host schema activated and not overridden,
  ;; Result obtains/does not obtain -> On/Off.
  ;; These go into CURRENT-STATE as they are not to be overridden.
  ;; (For efficiency, part of phase two is also done: if host is
  ;; overridden, put OFF as MAYBE-STATE).
  (dotimes (x *syn-item-number*)
    (let* ((syn (get-syn-item x))
	   (schema (get-schema (syn-item-host-schema syn))))
      (when (schema-activated-p schema)
	(if (schema-not-context-overridden-p schema)
	    (if (schema-result-satisfied-p schema)
		(setf (syn-item-current-state syn) (make-state-on)
		      (syn-item-unknown-time syn)  (fix+ *clock-tick*
							 (average-value
							   (syn-item-on-duration syn)))
		      (syn-item-set-time syn)      *clock-tick*)
		(setf (syn-item-current-state syn) (make-state-off)
		      (syn-item-unknown-time syn)  (fix+ *clock-tick*
							 (average-value
							   (syn-item-off-duration syn)))
		      (syn-item-set-time syn)      *clock-tick*))
	    (setf (syn-item-maybe-state syn) (make-state-off)))))))

(defun SYN-ITEM-UPDATE-PHASE-TWO-A ()
  ;; NOTE: the accidental clobbering of phase one values can be
  ;; prevented now by checking set-time --> if equal to *clock-tick*,
  ;; it has already been set and should not be further modified.
  ;; PHASE TWO
  ;; Overriden host schema -> Off in maybe-state (done in phase one).
  ;; Not overridden host schema with reliable applicable child
  ;;    gives ON in MAYBE-STATE (unless MAYBE-STATE set in phase one).
  (dotimes (x *schema-number*)
    (let* ((schema (get-schema x))
	   (data (schema-data schema)))
      (when (flag-falsep (schema-data-result-empty data))
	(let ((parent (get-schema (schema-parent schema))))
	  (when (and (schema-syn-item-p parent)
		     (schema-data-applicable-p data)
		     (weighted-rate-high-p (schema-reliability schema))
		     (state-unknown-p
		       (syn-item-maybe-state (schema-reifier parent)))
		     (schema-not-context-overridden-p schema))
	    (setf (syn-item-maybe-state (schema-reifier parent))
		  (make-state-on))))))))

(defun SYN-ITEM-UPDATE-PHASE-THREE ()
  ;; PHASE THREE
  ;; Reliable activated schemas with synthetic items in the result
  ;; indicate that the item should be turned ON/OFF.
  ;; Move phase two and phase three results up into current-state
  ;; if not blocked by already being set this time around.
  (dotimes (x *schema-number*)
    (let* ((schema (get-schema x))
	   (data (schema-data schema)))
      (when (and (schema-data-activated-p data)
		 (weighted-rate-high-p (schema-reliability schema))
		 (flag-falsep (schema-data-result-empty data)))
	(let ((result-item (schema-result-item schema)))
	  (if (schema-data-result-conj-p data)
	      (flag-array-ior (conj-inclusion-array
				(get-conj result-item))
			      *reliable-conj*
			      *fixna-required-to-hold-all-conj-flags*)
	      (setf (flag-array-get-flag
		      (if (schema-data-result-negated-p data)
			  *reliable-item-neg*
			  *reliable-item-pos*)
		      result-item)
		    (make-flag-true)))))))
  (dotimes (x *conj-number*)
    (when (flag-truep (flag-array-get-flag *reliable-conj* x))
      (let ((conj (get-conj x)))
	;; In the usual (original) case, *FIXNA-REQUIRED-TO-HOLD-ALL-ITEM-FLAGS*
	;; (and hence the length of *RELIABLE-ITEM-POS*) is smaller than
	;; *FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-FLAGS* (and hence the length of
	;; (CONJ-xxx-FLAG-ARRAY ...)).  However, in the case where I've boosted
	;; the maximum number of conj's from 300 to 3000, this is no longer the case.
	(let ((minimal-length (min *fixna-required-to-hold-all-conj-flags*
				   *fixna-required-to-hold-all-item-flags*)))
	  (flag-array-ior (conj-pos-flag-array conj)
			  *reliable-item-pos*
			  minimal-length)
	  (flag-array-ior (conj-neg-flag-array conj)
			  *reliable-item-neg*
			  minimal-length)))))
  (dotimes (x *item-number*)
    (let* ((item (get-item x))
	   (data (item-data item)))
      (when (item-data-syn-item-p data)
	(let ((neg (flag-array-get-flag *reliable-item-neg* x))
	      (pos (flag-array-get-flag *reliable-item-pos* x))
	      (syn (get-syn-item (item-syn-item-index item))))
	  (when (fix/= *clock-tick* (syn-item-set-time syn))
	    (if (flag-truep neg)
		(if (flag-truep pos)
		    ;; If both are true, set unknown.
		    (setf (syn-item-current-state syn)
			  (make-state-unknown)
			  (syn-item-set-time syn)
			  *clock-tick*)
		    ;; Neg true, pos false, if not marked in phase
		    ;; two, set OFF.
		    (if (state-unknown-p (syn-item-maybe-state syn))
			(setf (syn-item-current-state syn)
			      (make-state-off)
			      (syn-item-unknown-time syn)
			      (fix+ *clock-tick*
				    (average-value
				      (syn-item-off-duration syn)))
			      (syn-item-set-time syn)
			      *clock-tick*)
			;; Marked in phase two, set UNKNOWN.
			(setf (syn-item-current-state syn)
			      (make-state-unknown)
			      (syn-item-set-time syn)
			      *clock-tick*)))
		;; Neg false.
		(if (flag-truep pos)
		    ;; Neg false, pos true, if not marked in phase
		    ;; two, set ON.
		    (if (state-unknown-p (syn-item-maybe-state syn))
			(setf (syn-item-current-state syn)
			      (make-state-on)
			      (syn-item-unknown-time syn)
			      (fix+ *clock-tick*
				    (average-value
				      (syn-item-on-duration syn)))
			      (syn-item-set-time syn)
			      *clock-tick*)
			;; Marked in phase two, set unknown.
			(setf (syn-item-current-state syn)
			      (make-state-unknown)
			      (syn-item-set-time syn)
			      *clock-tick*))))))))))

(defun SYN-ITEM-UPDATE-PHASE-TWO-B ()
  ;; PHASE TWO REVISITED
  ;; If phase two and three conflicted, the current-state was set to
  ;; unknown -- any synthetic item which hasn't been set this cycle
  ;; and has a non-unknown MAYBE-STATE should use the MAYBE-STATE to
  ;; update current-state.
  (dotimes (x *syn-item-number*)
    (let ((syn (get-syn-item x)))
      (when (and (fix/= *clock-tick* (syn-item-set-time syn))
		 (state-noteq (make-state-unknown)
			      (syn-item-maybe-state syn)))
	(setf (syn-item-current-state syn) (syn-item-maybe-state syn)
	      (syn-item-unknown-time syn)  (fix+ *clock-tick*
						 (average-value
						   (if (state-on-p
							 (syn-item-maybe-state syn))
						       (syn-item-on-duration syn)
						       (syn-item-off-duration syn))))
	      (syn-item-set-time syn)      *clock-tick*)))))

(defun SYN-ITEM-UPDATE-PHASE-FOUR ()
  ;; PHASE FOUR
  ;; Timeouts.
  (dotimes (x *syn-item-number*)
    (let ((syn (get-syn-item x)))
      (when (and (fix/= *clock-tick* (syn-item-set-time syn))
		 (fix= *clock-tick* (syn-item-unknown-time syn)))
	(setf (syn-item-current-state syn) (make-state-unknown)
	      (syn-item-set-time syn)      *clock-tick*)))))

(defun SYN-ITEM-UPDATE-PHASE-FIVE ()
  ;; PHASE FIVE
  ;; Update the items and microworld-state array with the newly
  ;; calculated information -- also update generality for the
  ;; synthetic items.
  (dotimes (x *item-number*)
    (let ((current-item (get-item x)))
      (when (item-syn-item-p current-item)
	(let ((new-state
                (syn-item-current-state
		  (get-syn-item (item-syn-item-index current-item)))))
	  (setf (item-current-state current-item) new-state
		(get-microworld-state x)          new-state)
	  (item-generality-update current-item (state-on-p new-state)))))))

;;;; Major functions:  SCHEMA-UPDATE-ALL-RESULTS and SCHEMA-UPDATE-RELIABILITY.

(defun SCHEMA-UPDATE-ALL-RESULTS ()
  (with-schema-update-framework (result "all results")
				("~4D ~25A~A"
				 schema-index
				 (schema-print-name schema)
				 (flag-unparse (schema-result-satisfied schema)))
    (setf (schema-result-satisfied schema)
	  (flag-parse
	    (or (schema-result-empty-p schema)
		(if (schema-result-conj-p schema)
		    (conj-satisfied-p
		      (schema-result-item schema))
		    (state-eq
		      (if (schema-result-negated-p schema)
			  (make-state-off)
			  (make-state-on))
		      (get-microworld-state (schema-result-item schema)))))))))

(defun SCHEMA-UPDATE-RELIABILITY ()
  (without-floating-underflow-traps		; New:  This bombed out once due to extremely tiny reliability...
    (with-schema-update-framework (reliability "reliability")
				  ("~4D ~6,4F"
				   schema-index
				   (schema-reliability schema))
      (when (schema-activated-p schema)
	(weighted-rate-update (schema-reliability schema)
			      (schema-result-satisfied-p schema))))))

;;;; Predicted-results definitions.

(deflimit *PREDICTED-RESULTS-SIZE*       *fixna-required-to-hold-all-item-states* 25)
(deflimit *PREDICTED-RESULTS-CONJS-SIZE* *fixna-required-to-hold-all-conj-states* 20)

(defvar *PREDICTED-RESULTS*
	(make-state-array *predicted-results-size*))

(defvar *PREDICTED-RESULT-CONJS*
	(make-state-array *predicted-results-conjs-size*))

(defmacro PREDICTED-RESULT (item)
  `(state-noteq (make-state-unknown)
                (state-array-get-state *predicted-results* ,item)))

(defmacro PREDICTED-RESULT-CONJ (conj)
  `(state-noteq (make-state-unknown)
                (state-array-get-state *predicted-result-conjs*
                                       ,conj)))

;;;; major functions:  UPDATE-PREDICTED-RESULTS and PRINT-PREDICTED-RESULTS.

(defun UPDATE-PREDICTED-RESULTS ()
  (state-array-clear *predicted-results* *predicted-results-size*)
  (state-array-clear *predicted-result-conjs* *predicted-results-conjs-size*)
  (dotimes (schema-index *schema-number*)
    (let ((schema (get-schema schema-index)))
      (when (and (flag-falsep (schema-result-empty schema))
		 (weighted-rate-high-p (schema-reliability schema))
		 (schema-activated-p schema)
		 (schema-result-satisfied-p schema))
	(cond ((schema-result-conj-p schema)
	       (setf (state-array-get-state *predicted-result-conjs*
					    (schema-result-item schema))
		     (if (schema-result-negated-p schema)
			 (make-state-off)
			 (make-state-on)))
	       (state-array-ior
		 (conj-item-array
		   (get-conj (schema-result-item schema)))
		 *predicted-results*
		 *predicted-results-size*))
	      (t
	       (setf (state-array-get-state *predicted-results*
					    (schema-result-item schema))
		     (if (schema-result-negated-p schema)
			 (make-state-off)
			 (make-state-on)))))))))

(defun PRINT-PREDICTED-RESULTS-ENABLED ()
  (predicted-results-format "predicted-results~%")
  (dotimes (x *item-number*)
    (let ((foo (state-array-get-state *predicted-results* x)))
      (cond ((state-unknown-p foo)		; $OPT:  UNLESS might be clearer.
	     nil)
	    (t
	     (predicted-results-format
	       "~A~%"
	       (item-print-name (get-item x)))))))
  (predicted-results-format "predicted-result-conjunctions~%")
  (dotimes (x *conj-number*)
    (let ((foo (state-array-get-state *predicted-result-conjs* x)))
      (cond ((state-unknown-p foo)		; $OPT:  UNLESS might be clearer.
	     nil)
	    (t
	     (predicted-results-format
	       "~A~%"
	       (conj-print-name (get-conj x))))))))

;;;; Major function:  SCHEMA-UPDATE-EXT-ITEM-STATS.

(defun SCHEMA-UPDATE-EXT-ITEM-STATS ()
  (dotimes (item-index *item-number*)
    (let ((current-item (get-item item-index))
	  (record-offset 0))
      (multiple-value-bind (array-index record-position)
	  (get-counter-array-index item-index)
        (setq record-offset (counter-record-offset record-position))
        (item-format "~A~%" current-item)
	(with-schema-update-framework-innards ext-stats
					      (nil)
	  (cond ((schema-result-empty-p schema)
		 (let ((activated (schema-activated schema))
		       (positive (schema-extended-result-pos schema))
		       (negative (schema-extended-result-neg schema))
		       (current-state (item-current-state current-item))
		       (last-state (item-last-state current-item)))
		   (ext-stats-format "~4D res " schema-index)
		     ;;; Do positive transition -- activation matches and old is off.
		   (cond ((and (state-off-p last-state)
			       (not (and
				      (flag-falsep activated)
				      (predicted-result item-index)))
			       (flag-eq
				 (counter-array-toggle
				   positive array-index record-offset)
				 activated))
			  (ext-stats-format "+t ")
			    ;;; Check to see if new is on.
			  (cond ((state-on-p current-state)
				 (ext-stats-format "~A->"
						   (counter-unparse-from-array
						     positive array-index record-offset))
				 (counter-array-toggle-toggle
				   positive array-index record-offset)
				 (counter-array-modify-value
				   positive array-index record-offset activated)
				 (ext-stats-format "~A"
						   (counter-unparse-from-array
						     positive array-index record-offset)))
				(t
				 (ext-stats-format "~A->"
						   (counter-unparse-from-array
						     positive array-index record-offset))
				 (counter-array-toggle-toggle
				   positive array-index record-offset)
				 (ext-stats-format "~A"
						   (counter-unparse-from-array
						     positive array-index record-offset)))))
			   ;;; Do negative transition -- old is on.
			 ((and (state-on-p last-state)
			       (not (and (flag-falsep activated)
					 (predicted-result item-index)))
			       (flag-eq
				 (counter-array-toggle
				   negative array-index record-offset)
				 activated))
			  (ext-stats-format "-t ")
			    ;;; Check to see if new is off.
			  (cond ((state-off-p current-state)
				 (ext-stats-format "~A->"
						   (counter-unparse-from-array
						     negative array-index record-offset))
				 (counter-array-toggle-toggle
				   negative array-index record-offset)
				 (counter-array-modify-value
				   negative array-index record-offset activated)
				 (ext-stats-format "~A"
						   (counter-unparse-from-array
						     negative array-index record-offset)))
				(t
				 (ext-stats-format "~A->"
						   (counter-unparse-from-array
						     negative array-index record-offset))
				 (counter-array-toggle-toggle
				   negative array-index record-offset)
				 (ext-stats-format "~A"
						   (counter-unparse-from-array
						     negative array-index record-offset)))))
			 (t
			  (ext-stats-format "no ")))))
		(t
		 ;; Non-empty result so update extended-context.
		 (ext-stats-format "~4D con " schema-index)
		 (cond ((and (schema-activated-p schema)
			     (or
			       (state-unknown-p
				 (state-array-get-state
				   (schema-context-children schema) item-index))
			       (state-noteq
				 (state-array-get-state
				   (schema-context-children schema) item-index)
				 (item-last-state current-item))))
			(let* ((ext-context (schema-extended-context schema))
			       (last-state (item-last-state current-item))
			       (toggle (counter-array-toggle
					 ext-context array-index record-offset)))
			  (cond ((or (and (flag-truep toggle)
					  (state-on-p last-state))
				     (and (flag-falsep toggle)
					  (state-off-p last-state)))
				 (ext-stats-if
				   (state-on-p last-state)
				   (ext-stats-format "+t ")
				   (ext-stats-format "-t "))
				 (ext-stats-format
				   "~A->"
				   (counter-unparse-from-array
				     ext-context array-index record-offset))
				 (counter-array-toggle-toggle
				   ext-context array-index record-offset)
				 (if (schema-result-satisfied-p schema)
				     (counter-array-modify-value
				       ext-context array-index record-offset
				       (flag-parse (state-on-p last-state))))
				 (ext-stats-format
				   "~A "
				   (counter-unparse-from-array
				     ext-context array-index record-offset)))
				(t
				 (ext-stats-format "no match state/toggle")))))
		       (t
			(ext-stats-format "not activated or deferred"))))))
	(ext-stats-format "~%")))))

;;;; Major function:  SCHEMA-UPDATE-EXT-CONJ-STATS.

(defun SCHEMA-UPDATE-EXT-CONJ-STATS ()
  (loop for conj-index from 0 below *conj-number* do	; Changed from DOTIMES due to weird Genera compiler bug w/DOTIMES & FORMAT???
    (let* ((current-conj (get-conj conj-index))
	   (record-offset 0)
	   (current-state (conj-current-state current-conj))
	   (last-state (conj-last-state current-conj)))
      (multiple-value-bind (array-index record-position)
	  (get-counter-array-index conj-index)
	(setq record-offset (counter-record-offset record-position))
	(conj-format "~A~%" current-conj)
	(with-schema-update-framework-innards ext-conj-stats
					      (nil)
	  (ext-conj-stats-format "~4D res conj " schema-index)
	  (cond ((schema-result-empty-p schema)
		 (let ((activated (schema-activated schema))
		       (positive (schema-extended-result-conj-pos schema)))
		     ;;; Do positive transition -- activation matches and old is off.
		   (cond ((and (state-off-p last-state)
			       (not (and
				      (flag-falsep activated)
				      (predicted-result-conj conj-index)))
			       (flag-eq
				 (counter-array-toggle
				   positive array-index record-offset)
				 activated))
			  (ext-conj-stats-format "+t ")
                      ;;; check to see if new is on
			  (cond ((state-on-p current-state)
				 (ext-conj-stats-format
				   "~A->" (counter-unparse-from-array
					    positive array-index record-offset))
				 (counter-array-toggle-toggle
				   positive array-index record-offset)
				 (counter-array-modify-value
				   positive array-index record-offset activated)
				 (ext-conj-stats-format
				   "~A" (counter-unparse-from-array
					  positive array-index record-offset)))
				(t
				 (ext-conj-stats-format
				   "~A->" (counter-unparse-from-array
					    positive array-index record-offset))
				 (counter-array-toggle-toggle
				   positive array-index record-offset)
				 (ext-conj-stats-format
				   "~A" (counter-unparse-from-array
					  positive array-index record-offset)))))
			 (t (ext-conj-stats-format "no ")))))
		(t
		 (ext-conj-stats-format "-- res non-empty"))))))
    (ext-conj-stats-format "~%")))

;;;; Major function:  MAYBE-SPINOFF-SCHEMA.
     
(defun MAYBE-SPINOFF-SCHEMA ()
  (dotimes (schema-index *schema-number*)
    (when ; Returns 'MAYBE-SPINOFF-SCHEMA-FINISHED if clause true.  What's the point?
      (let ((schema (get-schema schema-index))
	    (array-index 0)
	    (record-offset 0))
	(if (schema-result-empty-p schema)
	    (let ((result-pos
		    (schema-extended-result-pos schema))
		  (result-neg
		    (schema-extended-result-neg schema))
		  (result-pos-conj
		    (schema-extended-result-conj-pos schema))
		  (children (schema-result-children schema))
		  (conj-children (schema-result-conj-children schema)))
	      (dotimes (item-index *item-number* nil)
		(if (state-unknown-p
		      (state-array-get-state children item-index))
		    (cond ((and (fix= *counter-maximum*
				      (counter-array-value
					result-pos array-index record-offset))
				(flag-truep
				  (counter-array-pos
				    result-pos array-index record-offset)))
			   (return
			     (make-spinoff-result schema-index
						  item-index
						  (make-state-on))))
			  ((and (fix= *counter-maximum*
				      (counter-array-value
					result-neg array-index record-offset))
				(flag-truep
				  (counter-array-pos
				    result-neg array-index record-offset)))
			   (return
			     (make-spinoff-result schema-index
						  item-index
						  (make-state-off))))
			  (t nil)))
		(if (and (fix< item-index *conj-number*)
			 (flag-falsep
			   (flag-array-get-flag conj-children item-index)))
		    (cond ((and (fix= *counter-maximum*
				      (counter-array-value
					result-pos-conj array-index record-offset))
				(flag-truep
				  (counter-array-pos
				    result-pos-conj array-index record-offset)))
			   (return
			     (make-spinoff-result-conj
			       schema-index
			       item-index)))
			  (t nil)))
		(if (fix= *counter-record-max-offset* record-offset)
		    (setq record-offset 0
			  array-index (fix1+ array-index))
		    (setq record-offset (fix+ *counter-bits* record-offset)))))
	    ;; Non-empty result so check for context spinoffs.
	    (let ((context (schema-extended-context schema))
		  (children (schema-context-children schema))
		  (current-item -1)
		  (current-state (make-state-unknown)))
	      (dotimes (item-index *item-number*
				   (if (state-noteq (make-state-unknown) current-state)
				       (make-spinoff-context schema-index
							     current-item
							     current-state)
				       nil))
		(if (and (state-unknown-p
			   (state-array-get-state children item-index))
			 (fix= *counter-maximum*
			       (counter-array-value
				 context array-index record-offset))
			 (or (state-unknown-p current-state)
			     (item-generality-< (get-item item-index)
						(get-item current-item))))
		    (setq current-state
			  (if (flag-truep
				(counter-array-pos
				  context array-index record-offset))
			      (make-state-on)
			      (make-state-off))
			  current-item item-index))
		(if (fix= *counter-record-max-offset* record-offset)
		    (setq record-offset 0
			  array-index (fix1+ array-index))
		    (setq record-offset (fix+ *counter-bits* record-offset)))))))
      (return 'maybe-spinoff-schema-finished))))

;;; Major function:  MAKE-SPINOFF-RESULT.

;;; As result spinoffs can only occur from empty-result schemas, and
;;; empty result schemas must necessarily have empty contexts, this
;;; routine does not have to deal with anything but spinning off from
;;; "blank" schemas -- i.e. those which do not have any context or
;;; result -- remember, all flags are false by default.

(defun MAKE-SPINOFF-RESULT (schema item state)	; SCHEMA and ITEM are really indices, not actual objects...
  (let* ((parent-schema (get-schema schema))
         (spinoff-schema (make-action-schema
			   (schema-action-item parent-schema))))
    (setf (schema-result-item spinoff-schema)   item
          (schema-context-empty spinoff-schema) (make-flag-true)
          (schema-parent spinoff-schema)        schema)
    (when (state-off-p state)
      (setf (schema-result-negated spinoff-schema)
	    (make-flag-true)))
    (setf (bit (item-result-dependent-schemas (get-item item))	; Note that this item now has a schema which uses it in its result.
	       (1- *schema-number*))		; The schema is the NEW schema, _not_ the (bare) schema index in SCHEMA!
	  1)
    (schema-update-print-name spinoff-schema)
    (main-format "~5D spinoff-result  ~A~6A ~A -> ~A~%"
                 *clock-tick*
                 (state-unparse state)
                 (item-print-name (get-item item))
                 (schema-print-name parent-schema)
                 (schema-print-name spinoff-schema))
    (setf (state-array-get-state
	    (schema-result-children parent-schema) item)
	  state)
    spinoff-schema))

;;;; Major Function:  MAKE-SPINOFF-RESULT-CONJ.

(defun CHECK-ITEM-RESULTS ()			; DBG.
  (loop for index from 0 below *item-number*
	for item = (get-item index)
	  for r = (item-result-dependent-schemas item)
	  do (format t "~&~3D  ~S~&"
		     index
		     (enumerate-bit-vector r)))
  (values))

(defun CHECK-ITEM-CONTEXTS ()			; DBG.
  (loop for index from 0 below *item-number*
	for item = (get-item index)
	  for c = (item-context-dependent-schemas item)
	  do (format t "~&~3D  ~S~&"
		     index
		     (enumerate-bit-vector c)))
  (values))

(defmacro TELLING-ITEMS-ABOUT-SCHEMA (item-dependency-array item-state-array schema-index)
  ;; For each item in the state-array whose state is not unknown (e.g., positively or negatively included),
  ;; mark each such item with the schema, so the item knows the schema is dependent upon it.
  `(loop for state-index from 0 below *item-number*
	 do (unless (state-unknown-p (state-array-get-state ,item-state-array state-index))
	      ;; The item number represented by STATE-INDEX is included (either positively or negatively) in this conj.
	      ;; This means that we must tell the item that SCHEMA is referencing it.
	      (setf (bit (,item-dependency-array (get-item state-index)) ,schema-index) 1))))

(defun TELL-ITEMS-ABOUT-SCHEMA-RESULT-DEPENDENCY (item-state-array schema-index)
  (telling-items-about-schema item-result-dependent-schemas  item-state-array schema-index))

(defun TELL-ITEMS-ABOUT-SCHEMA-CONTEXT-DEPENDENCY (item-state-array schema-index)
  (telling-items-about-schema item-context-dependent-schemas item-state-array schema-index))

(defun MAKE-SPINOFF-RESULT-CONJ (schema conj)	; SCHEMA and CONJ are really indices, not actual objects...
  (let* ((parent-schema (get-schema schema))
         (spinoff-schema (make-action-schema
			   (schema-action-item parent-schema))))
    (setf (schema-result-item   spinoff-schema) conj
          (schema-result-conj   spinoff-schema) (make-flag-true)
          (schema-context-empty spinoff-schema) (make-flag-true)
          (schema-parent        spinoff-schema) schema)
    (tell-items-about-schema-result-dependency	; Note that these items now have schemas which use them in their results.
      (conj-item-array (get-conj conj))
      (1- *schema-number*))			; The schema is the NEW schema, _not_ the (bare) schema index in SCHEMA!
    (schema-update-print-name spinoff-schema)
    (main-format "~5D spinoff-result-conjunction  ~A~6A ~A -> ~A~%"
                 *clock-tick*
                 (state-unparse (make-state-on))
                 (conj-print-name (get-conj conj))
                 (schema-print-name parent-schema)
                 (schema-print-name spinoff-schema))
    (setf (flag-array-get-flag
	    (schema-result-conj-children parent-schema) conj)
	  (make-flag-true))
    spinoff-schema))

;;;; Major function:  MAKE-SPINOFF-CONTEXT.

;;; A context spinoff has same context as its parent, but with a new
;;; item added -- note that this invalidates any conjunction
;;; information, so this isn't copied (by default context-conjunction
;;; is false, and context-item is -1).
;;; RESULT and ACTION are direct copies from the parent.
;;; The EXTENDED-CONTEXT counters for the parent schema are also reset
;;;   (as required for deferring to a more specific schema).
;;; To inhibit context spinoffs of items which are already in the
;;; context, the CONTEXT-ARRAY is copied to the CONTEXT-CHILDREN array.
    
(defun MAKE-SPINOFF-CONTEXT (schema item state)	; SCHEMA and ITEM are really indices, not actual objects...
  (let* ((parent-schema (get-schema schema))	; The schema that already exists, from which we are spinning off a new one.
         (spinoff-schema (make-action-schema	; The new, spinoff schema (a real object, not an index).
			   (schema-action-item parent-schema)))
         (parent-schema-data (schema-data parent-schema)))
    (if (schema-data-context-empty-p parent-schema-data)
        (setf (schema-context-single spinoff-schema) (make-flag-true))
	(state-array-copy (schema-context-array parent-schema)
			  (schema-context-array spinoff-schema)
			  *fixna-required-to-hold-all-item-states*))
    (setf (state-array-get-state (schema-context-array spinoff-schema) item) state
          (schema-parent spinoff-schema) schema)
    (state-array-copy (schema-context-array spinoff-schema)
                      (schema-context-children spinoff-schema)
                      *fixna-required-to-hold-all-item-states*)
    (if (schema-data-result-empty-p parent-schema-data)
        (setf (schema-result-empty spinoff-schema)   (make-flag-true))
	(setf (schema-result-item spinoff-schema)    (schema-result-item parent-schema)
	      (schema-result-conj spinoff-schema)    (schema-data-result-conj parent-schema-data)
	      (schema-result-negated spinoff-schema) (schema-data-result-negated parent-schema-data)))
    (setf (state-array-get-state (schema-context-children parent-schema) item) state
          (schema-extended-context parent-schema)        (make-counter-array *fixna-required-to-hold-all-item-counters*))
    (when (schema-data-action-gd-p parent-schema-data)
      (setf (schema-extended-context-post parent-schema) (make-counter-array *fixna-required-to-hold-all-item-counters*)))
    (let ((spinoff-schema-index (1- *schema-number*)))	; The index of the newly-created schema (*SCHEMA-NUMBER* always points to first UNUSED index).
      ;; Note that these items now have schemas which use them in their contexts (from the parent schema).
      (tell-items-about-schema-context-dependency
	(schema-context-array parent-schema)
	spinoff-schema-index)
      ;; ALSO, add the NEW context item (from the spinoff schema).
      (setf (bit (item-context-dependent-schemas (get-item item))
		 spinoff-schema-index) 1)
      ;; ALSO, note that the items in the new schema's RESULT are dependents of it!
      (cond ((schema-result-conj-p spinoff-schema)	; Schema is a result-conj, so update for each item in the conj.
	     (let* ((conj-index (schema-result-item spinoff-schema))
		    (conj (get-conj conj-index))
		    (conj-item-state-array (conj-item-array conj)))
	       (tell-items-about-schema-result-dependency
		 conj-item-state-array spinoff-schema-index)))
	    (t					; Schema is ordinary result, so update for the single item.
	     (setf (bit (item-result-dependent-schemas (get-item (schema-result-item spinoff-schema)))
			spinoff-schema-index)
		   1))))
    (schema-update-print-name spinoff-schema)
    (main-format "~5D spinoff-context  ~A~6A ~A -> ~A~%"
                 *clock-tick*
                 (state-unparse state)		; * for ON, . for OFF
                 (item-print-name (get-item item))	; E.g., VF34.
                 (schema-print-name parent-schema)	; E.g., -VF04/EYEF/VF33.
                 (schema-print-name spinoff-schema))	; E.g., -VF04&VF34/EYEF/VF33.
    spinoff-schema))

;;;; Major function:  MAYBE-MAKE-CONJS.

(defun MAYBE-MAKE-CONJS ()
  (conj-format "making necessary conjunctions...~%")
  (dotimes (x *schema-number*)
    (let* ((schema (get-schema x))
	   (data (schema-data schema)))
      (when (and (flag-falsep (schema-data-context-single data))
		 (flag-falsep (schema-data-context-empty data))
		 (weighted-rate-high-p (schema-reliability schema))
		 (flag-falsep (schema-data-context-conj data)))
	(setf (schema-context-item schema) (make-conj (schema-context-array schema))
	      (schema-context-conj schema) (make-flag-true))
	(schema-update-print-name schema)
	(conj-format "modifying ~4D ~A conj ~D~%"
		     x
		     (schema-print-name schema)
		     (schema-context-item schema)))))
  (dotimes (x *conj-number*)
    x   ; Ignore this to avoid a compiler warning, in case CONJ-FORMAT is expanding to NIL.  (I had to say (IGNORE X) instead of just X.  Why???)
    (conj-format "~A~%" (get-conj x))))

;;;; Major function:  MAYBE-MAKE-SYN-ITEMS.

;;; A schema succeeds if, when activated, its result obtains.
;;; Initially, a schema updates the LC-CONSY rate, if it
;;; succeeded this time -- then check to see if succeeded last time
;;; and update rate accordingly.
;;; When highly lcly-cons and not reliable, stop updating
;;; lc-cons and start updating durations.
;;; ON-DURATION:  the duration from the first successful execution to
;;; the first unsuccessful one.
;;; OFF-DURATION:  the duration from the first unsuccessful execution to
;;; the first successful one.

(defun MAYBE-MAKE-SYN-ITEMS ()
  (syn-item-format
    "making synthetic items and updating duration/consistency...~%")
  (loop for schema-index from 0 below *schema-number*	; Changed from DOTIMES due to weird Genera compiler bug w/DOTIMES & FORMAT???
	do					; [ ... but _this_ one wasn't required until 18-Aug-93, long after all the rest!  ...and just after ...
    (syn-item-format "~4D " schema-index)	;   ... I macro-ized all of the microworld stuff, but before fixing the *SCHEMA-NUMBER* = 0 bug... Hmm...]
    (let ((schema (get-schema schema-index)))
      (cond ((schema-lcly-cons-p schema)
	     (if (schema-activated-p schema)
		 (cond ((and (schema-result-satisfied-p schema)
			     (not (schema-succeeded-last-p schema)))
			(syn-item-format "syn update off-duration before ~3D "
					 (average-value
					   (syn-item-off-duration
					     (get-syn-item
					       (schema-reifier schema)))))
			(average-update
			  (syn-item-off-duration
			    (get-syn-item (schema-reifier schema)))
			  (fix- *clock-tick* (schema-first-tick schema)))
			(syn-item-format "after ~3D "
					 (average-value
					   (syn-item-off-duration
					     (get-syn-item
					       (schema-reifier schema)))))
			(syn-item-format "first-tick set ~5D" *clock-tick*)
			(setf (schema-first-tick schema) *clock-tick*))
		       (t
			(when (and (not (schema-result-satisfied-p schema))
				   (schema-succeeded-last-p schema))
			  (syn-item-format "syn update on-duration before ~3D "
					   (average-value
					     (syn-item-on-duration
					       (get-syn-item
						 (schema-reifier schema)))))
			  (average-update
			    (syn-item-on-duration
			      (get-syn-item (schema-reifier schema)))
			    (fix- *clock-tick* (schema-first-tick schema)))
			  (syn-item-format "after ~3D "
					   (average-value
					     (syn-item-on-duration
					       (get-syn-item
						 (schema-reifier schema)))))
			  (syn-item-format "first-tick set ~5D" *clock-tick*)
			  (setf (schema-first-tick schema) *clock-tick*))))
		 (syn-item-format "syn not activated ")))
	    (t
	     ;; Not locally consistent.
	     (cond ((and (schema-activated-p schema)
			 (flag-falsep (schema-result-empty schema))
			 (schema-result-satisfied-p schema))
		    (syn-item-format "no syn update consistency before ~A "
				     (schema-lc-consy-unparse schema))
		    (schema-lc-consy-update
		      schema (schema-succeeded-last-p schema))
		    (syn-item-format "after ~A "
				     (schema-lc-consy-unparse schema))
		    (when (and (schema-lc-consy-high-p schema)
			       (weighted-rate-low-p (schema-reliability schema)))
		      (setf (schema-lcly-cons schema) (make-flag-true)
			    (schema-reifier schema)   (make-syn-item schema-index))))
		   (t
		    (syn-item-format
		      "no syn not activated or no non-empty satisfied result")))))
      (syn-item-format "~%")
      (when (schema-activated-p schema)
	(setf (schema-succeeded-last schema)
	      (schema-result-satisfied schema))))))

;;;; Summary reporting.

;;; SHOW-RELIABLE-SCHEMAS is used by RUN-MICROWORLD, but the other definitions
;;; might be useful elsewhere, so we'll define them here.  I'm using substs instead
;;; of macros so they can be used functionally, too.  Note that I haven't covered the
;;; entire set, but merely partitioned it into those whose reliability is above
;;; *WEIGHTED-RATE-HIGH* and those below.  There are other ranges between high
;;; and medium, and between medium and low, and below low, that could conceivably
;;; be useful as well.

(defsubst RELIABLE-SCHEMA? (schema)
  (weighted-rate-high-p (schema-reliability schema)))

(defsubst RELIABLE-SCHEMA-INDEX? (schema-index)
  (reliable-schema? (get-schema schema-index)))

;;; What the name says:  not reliable.  Could be semireliable or totally worthless. 
(defsubst UNRELIABLE-SCHEMA? (schema)
  (not (reliable-schema? schema)))

(defsubst UNRELIABLE-SCHEMA-INDEX? (schema-index)	; Ditto.
  (not (reliable-schema-index? schema-index)))

(defun COUNT-RELIABLE-SCHEMAS (&optional (from 0) (below *schema-number*))
  (loop for index from from below below
	count (reliable-schema-index? index)))

;;; Used by the iteration logging code.
(defun SHOW-RELIABLE-SCHEMAS (&optional iteration dont-list-em (from 0) (below *schema-number*))
  ;; If ITERATION is supplied, we'll talk about what iteration this is.
  ;; If DONT-LIST-EM is supplied, we won't bother listing exactly what
  ;; the reliable schemas are, though.
  (main-format "~A~:[~;Iteration ~:*~D.  ~]~D total schema~:P, ~D reliable schema~:P~
                ~:[ in the range from ~D below ~D~;~2*~]~:[:~;.~]~&"
	       (emit-date-prefix nil nil)
	       iteration
	       *schema-number*
	       (count-reliable-schemas from below)
	       (and (= from 0) (= below *schema-number*))
	       from below
	       dont-list-em)
  (unless dont-list-em
    (loop with printed-counter = 0
	  for index from from below below
	  when (reliable-schema-index? index)
	    do (when (and (zerop (mod printed-counter 25))	; 25 schemas per line.
			  (not (zerop printed-counter)))	; If no reliable schemas yet, don't do a zillion newlines!
		 (main-format "~%")
		 (setf printed-counter 0))
	       (main-format "~D " index)	; No point in forcing wide columns...
	       (incf printed-counter))
    (main-format "~%")))

;;; For exploration.  Not designed to be particularly efficient...
(defun ALL-RELIABLE-SCHEMA-NUMBERS (&optional (from 0) (below *schema-number*) unreliable-instead)
  (loop for index from from below below
	when (or (and (not unreliable-instead)
		      (reliable-schema-index? index))
		 (and unreliable-instead
		      (not (reliable-schema-index? index))))
	  collect index))

;;; For exploration.  Not designed to be particularly efficient...
(defun ALL-RELIABLE-SCHEMAS (&optional (from 0) (below *schema-number*) unreliable-instead)
  (loop for index from from below below
	when (or (and (not unreliable-instead)
		      (reliable-schema-index? index))
		 (and unreliable-instead
		      (not (reliable-schema-index? index))))
	  collect (get-schema index)))

;;; For exploration.  Not used by the iteration logging code.
(defun SHOW-RELIABLE-SCHEMAS-IN-TABLE (&optional (from 0) (below *schema-number*) unreliable-instead)
  (let ((schemas (all-reliable-schemas from below unreliable-instead)))
    (loop for schema in schemas
	  do (format t "~&~A~&" schema)))
  (values))

;;;; Initialization of the schema mechanism.

;;; "Makes" all the items.  Many item functions have been defined, but this stuffs
;;; their names and function cells into the data structure that's actually examined by
;;; the schema mechanism, by calling MAKE-ITEM for each item.  We take care to do
;;; this in the order in which the DEFITEMs were originally compiled, because certain
;;; debugging functions (EYEHAND-SHOW-ITEMS-ENABLED, Ramstad's original function
;;; renamed from his name of SHOW-ITEMS, being the major offender) "know" which
;;; item number is which, and item numbers are assigned sequentially by MAKE-ITEM
;;; each time it's called.  (*sigh* Really, of course, we now have the information in
;;; *primitive-items* to make such functions look the information up instead.  The
;;; first time I need this functionality in a microworld whose dimensions are
;;; different from Ramstad's original microworld, I'll implement it.)
;;;
;;; ITEMS should be the given microworld's *PRIMITIVE-ITEMS* list, as generated by
;;; DEFITEM.  Returns the number of items.
(defun MAKE-ALL-ITEMS (items pkg)
  (setf *item-number* 0)
  (setf *syn-item-number* 0)
  (loop for name in (reverse items)
	do (make-item
	     (string-downcase (symbol-name name))
	     (symbol-function name)
	     pkg))
  *item-number*)

;;; Same as above, but for actions.
(defun MAKE-ALL-ACTIONS (actions pkg)
  (setf *action-number* 0)
  (loop for name in (reverse actions)
	do (make-blank-action-schema
	     (make-action (format nil "/~(~A~)/" (symbol-name name))
			  (symbol-function name)
			  pkg)))
  *action-number*)

;;; Clears various crucial counters that aren't cleared by more specific initialization
;;; routines.  Note that this has to run _before_ MAKE-ALL-ITEMS and _especially_
;;; MAKE-ALL-ACTIONS, because MAKE-ALL-ACTIONS also increments *SCHEMA-NUMBER*
;;; when it defines the bare action schemas.
(defun CLEAR-SOME-MECHANISM-COUNTERS ()
  (setf *clock-tick*    0)
  (setf *schema-number* 0)
  (setf *conj-number*   0)
  (values))

;;; End of file.
