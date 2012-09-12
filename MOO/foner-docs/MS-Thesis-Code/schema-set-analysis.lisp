;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

(in-package :schema)

;;;; Analyzing runs.

;;;; Exactly comparing the schemas generated from a pair of runs.

;;; Note that this stuff is pretty heavily Genera-dependent, though a less-efficient, non-Genera
;;; version of EQUALP could be written if required.

;;; +++ Substrate for getting the right conj arrays etc out of the snapshot.
;;; +++ See CONJ-EQUALP et al for a discussion of what a kluge this is.

(defvar *MY-EQUALP-AUX-BINDINGS-1* nil)
(defvar *MY-EQUALP-AUX-BINDINGS-2* nil)

;;; This is a terrible kluge!  It depends on the fact that the set operations apparently guarantee
;;; that items from sequence1 will be the first arg to the test function, and items from sequence2
;;; will be the second.  We use this to implement a working CONJ-EQUALP below.
(defmacro WITH-MY-EQUALP-AUX-BINDINGS ((snap1 snap2 schemas1 schemas2) &body body)	; SNAPn are snapshot numbers.
  `(let ((*my-equalp-aux-bindings-1* (schema-state ,snap1))
	 (*my-equalp-aux-bindings-2* (schema-state ,snap2)))
     (let ((,schemas1 (schema-state->schemas-as-list *my-equalp-aux-bindings-1*))
	   (,schemas2 (schema-state->schemas-as-list *my-equalp-aux-bindings-2*)))
       ,@body)))

(defmacro WITH-MY-EQUALP-AUX-BINDINGS-OP (snap1 snap2 operation &rest op-args)	; SNAPn are snapshot numbers.
  `(with-my-equalp-aux-bindings (,snap1 ,snap2 schemas1 schemas2)
     (funcall ,operation schemas1 schemas2 :test #'my-equalp ,@op-args)))

;;; It's very important that the order of these arguments gets preserved all the way down the
;;; call tree, from MY-EQUALP to SCHEMA-EQUALP to us, because that's the only way we can tell
;;; which conj-array to index into.  This is, of course, a horrible kluge, but the lack of backpointers
;;; in the data structures (a consequence of its FORTRAN-like style) leaves little choice without a
;;; rewrite.  See comments at WITH-MY-EQUALP-AUX-BINDINGS.
;;; WARNING:  Requires bindings from WITH-MY-EQUALP-AUX-BINDINGS!
(defun CONJ-EQUALP (index-one index-two)			; indices or conj's?  just numbers?  hmmm...  should be indices.
  (let ((one (aref (schema-state->conjs-as-vector *my-equalp-aux-bindings-1*) index-one))
	(two (aref (schema-state->conjs-as-vector *my-equalp-aux-bindings-2*) index-two)))
    (and (my-equalp (conj-item-array one)	; I probably don't need to check this one...
		    (conj-item-array two))
	 (my-equalp (conj-pos-flag-array one)
		    (conj-pos-flag-array two))
	 (my-equalp (conj-neg-flag-array one)
		    (conj-neg-flag-array two)))))

;;; What does "same" mean for two schemas?  Well, in this case, it means that they depend
;;; on the same context conditions and predict the same results.  It does NOT mean that they
;;; predict so with the same reliability, that the same other schemas depend on them, and so
;;; forth (e.g., other schemas might have been created in a different sequence, hence have
;;; different numbers, and we don't recursively walk the mess; also, the statistics being kept
;;; in the various EXTENDED- slots are probably going to differ, but we don't care about them).
;;; So note carefully which slots we check below and which we don't.
;;; Not sure:  DATA.
;;;
;;; Rationales:
;;;
;;; Checking CONTEXT-ITEM is not only redundant (in the non-conj case), but actually wrong
;;; (in the conj case).  CONTEXT-ITEM might or might not be a conj in the latter case, depending
;;; on whether we've run long enough to forma conj, but the items in the CONTEXT-ARRAY will
;;; be the same regardless.
;;;
;;; Checking RESULT-ITEM must take into account the fact that conj's might be created in different
;;; orders, hence just naively checking their indices will lose.  Instead, if this is a conj, we must
;;; compare the items that they include.
;;; 
;;; Uh, should I check REIFIER?  I see two close-to-equal (but not according to SCHEMA-EQUALP)
;;; schemas (/handf/-hp01), one with reifier 15 and one with reifier -1.  Do these count as being
;;; the same, or is it because one's supporting a synthetic item (and does this count?)  Note that
;;; having the reifier adds a child to CONTEXT-CHILDREN (which we don't currently check anyway).
;;;
;;; WARNING:  Requires bindings from WITH-MY-EQUALP-AUX-BINDINGS!
(defun SCHEMA-EQUALP (one two)
  (and (my-equalp (schema-print-name one)
		  (schema-print-name two))
       (my-equalp (schema-context-array one)
		  (schema-context-array two))
       (eq (schema-result-conj-p one)
	   (schema-result-conj-p two))
       (if (schema-result-conj-p one)
	   (conj-equalp (schema-result-item one)
			(schema-result-item two))
	   (my-equalp (schema-result-item one)	; This could probably just be = here...
		      (schema-result-item two)))
       (my-equalp (schema-action one)		; Probably redundant until gd-actions implemented.
		  (schema-action two))
       (my-equalp (schema-action-item one)	; Probably redundant until gd-actions implemented.
		  (schema-action-item two))
;      (my-equalp (schema-reifier one)		; Might this change?  Who's reifying whom here?
;                 (schema-reifier two))		; (It's possible that syn-items will be created out of order...  hmm...)
       ))

;;; +++ Version of EQUALP that uses the above.

;;; A version of EQUALP that ignores compiled-function definitions, since we write
;;; 'em out and read 'em in when we snapshot, and they come back with different
;;; addresses ('cause they're different objects now), but the same code.  It also
;;; calls SCHEMA-EQUALP on schemas.
;;; WARNING:  Requires bindings from WITH-MY-EQUALP-AUX-BINDINGS!
#+Genera
cli::
(defun MY-EQUALP (x y)
  (declare lt:(side-effects reader reducible))
  (declare (compiler:return-type boolean))
  (loop						; Um, why the HELL does this loop?  --- Foner.
    (cond ((eql x y) (return t))
	  ((numberp x) (return (and (numberp y) (= x y))))
	  ((not (= (sys:%data-type x) (sys:%data-type y))) (return nil))
	  ((functionp x)			; New.
	   (return (and (functionp y)
			(my-equalp (sys:function-name x)
				   (sys:function-name y)))))
	  ((consp x)
	   (loop
	     (unless (or (eql (car x) (car y))
			 (my-equalp (car x) (car y)))
	       (return-from my-equalp nil))
	     (setq x (cdr x) y (cdr y))
	     (unless (and (consp x) (consp y))
	       (return))))
	  ((characterp x) (return (and (characterp y) (char-equal x y))))
	  ((and (arrayp x) (arrayp y)		; New.  Do fast checks first.
		(typep x 'schema::schema) (typep y 'schema::schema))	; New.  Do slower checks second.
	   (return (schema::schema-equalp x y)))	; New.
	  ((arrayp x) (return (my-array-equalp x y)))
	  ((hash-table-p x)
	   (return (and (hash-table-p y) (hash-table-equalp x y)))) ; Unchanged for the moment.  In the general case, it should be changed.
	  ((instancep x)
	   (return (and (instancep y) (clos-internals::clos-instance-equalp x y))))	; Ditto.
	  (t (return nil)))))

#+Genera
cli::
(defun MY-ARRAY-EQUALP (a b)
  (let ((nd1 (array-rank a))
	(nd2 (array-rank b)))
    (compiler:%error-unless (= nd1 nd2)
      (return-from my-array-equalp nil))
    (cond ((= nd1 1)
	   (let ((length (length a)))		;hack fill pointers
	     (and (= (length b) length)
		  (let ((a a) (b b))
		    (declare (sys:array-register a b))
		    (dotimes (i length t)
		      (compiler:%error-unless (my-equalp (%1d-aref a i) (%1d-aref b i))
			(return nil)))))))
	  ((= nd1 0) (my-equalp (aref a) (aref b)))
	  ((not (loop for axis below nd1
		      always (= (array-dimension a axis) (array-dimension b axis))))
	   nil)
	  ((and (zerop (sys:array-discontiguous-bit a))
		(zerop (sys:array-discontiguous-bit b)))
	   (let ((a a) (b b))
	     (declare (sys:array-register-1d a b))
	     (dotimes (i (array-total-size a) t)
	       (compiler:%error-unless (my-equalp (%1d-aref a i) (%1d-aref b i))
		 (return nil)))))
	  (t
	   (with-stack-list (indices 0 0 0 0 0 0 0 0 0 0)	;up to 10, only support ~7
	     (with-stack-list (limits 0 0 0 0 0 0 0 0 0 0)	;up to 10, only support ~7
	       (setq indices (nthcdr (- 10. nd1) indices)
		     limits  (nthcdr (- 10. nd1) limits))
	       (loop for axis below nd1
		     for limit on limits
		     do (setf (car limit) (array-dimension a axis)))
	       (my-recursive-array-equalp a b indices indices limits)))))))

;;; In days gone by (before 10/17/85), this used to traverse the array
;;; in column-major order.  Sink your teeth into that!  This one is
;;; row-major.  It is always called with at least one axis to go.
#+Genera
cli::
(defun MY-RECURSIVE-ARRAY-EQUALP (a b indices indices-tail limits-tail)
  (dotimes (i (car limits-tail) t)
    (setf (car indices-tail) i)
    (unless (if (cdr indices-tail)
		(my-recursive-array-equalp a b indices (cdr indices-tail) (cdr limits-tail))
		(my-equalp (apply #'aref a indices) (apply #'aref b indices)))
      (return nil))))

#+Genera
(scl:deff my-equalp 'cli::my-equalp)		; For convenience.

;;;; The set relations that we're actually trying to do.

;;; Ugh.  Directly comparing schemas might be difficult (unless I maybe look at their
;;; pnames or something instead?), because, if conjs were generated in different orders
;;; or something, they resulting schemas might look at different things...  etc.  Hmm.
;;; Let's try a quick test regardless.  [Uh, "quick" is relative:  Each set operation below
;;; takes about 8 minutes on 3600-schema lists, for a total runtime of around 24-26 minutes.]
(defun SET-RELATIONS-SCHEMAS-ONLY (one two)		; Snapshot numbers.
  (values (with-my-equalp-aux-bindings-op one two #'intersection)
	  (with-my-equalp-aux-bindings-op one two #'set-difference)
	  (with-my-equalp-aux-bindings-op two one #'set-difference)))

(defvar *BUNCH-SET-RELATIONS* nil)

(defun BUNCH-SET-RELATIONS (&optional (combos '((0 1) (1 2) (0 2))))
  (length (setf *bunch-set-relations*
		(loop for (one two) in combos
		      collect (multiple-value-list
				(set-relations-schemas-only one two))))))

#+Genera					; Character styles & special characters.
(defun SUMMARIZE-BUNCH-DATA (&key
			     (bunch *bunch-set-relations*)
			     (combos '((0 1) (1 2) (0 2)))
			     (labels '((0 "Attentive Changes")
				       (1 "Changed Items All Schemas")
				       (2 "Full Crossbar"))))
  (flet ((lookup (run)
	   (second (assoc run labels))))
    (loop for counter from 0
	  for (intersection one-two two-one) in bunch	; I suppose I could use BUNCH-SELECT, but what the hell...
	  for combo in combos
	  for one = (first combo)
	  for two = (second combo)
	  unless (zerop counter)
	    do (format t "~2&")
	  do (safe-format t "~&~'b~A~ vs ~'b~A~:~&~
                               ~2T~D ~D ( ~D ~D):  ~D~&~
                               ~2T~D ~D (- ~D ~D):  ~D~&~
                               ~2T~D ~D (- ~D ~D):  ~D~&"
			  ;; Note that ITALIC-FORMAT above would require breaking up the string to use,
			  ;; and I'd still have to work around the intersection character.  I just don't care.
			  (lookup one) (lookup two)
			  counter 0 one two (length intersection)
			  counter 1 one two (length one-two)
			  counter 2 two one (length two-one))))
  (values))

(defun BUNCH-SELECT (combo operation &optional (bunch *bunch-set-relations*))
  (nth operation (nth combo bunch)))

(defun COMPARE-BUNCH-PIECES (combo1 operation1 combo2 operation2 start end &optional (bunch *bunch-set-relations*))
  (show-schemas-list (bunch-select combo1 operation1 bunch) :start start :end end)
  (format t "~2&")
  (show-schemas-list (bunch-select combo2 operation2 bunch) :start start :end end)
  (values))

;;; Some more ideas on comparing & sorting schemas.

(defun ALL-SCHEMAS-IN-BUNCH (&optional (bunch *bunch-set-relations*))
  (loop for counter from 0 below (length bunch)
	append (bunch-select counter 0)
	append (bunch-select counter 1)
	append (bunch-select counter 2)))

;;; This'll only ever work for schemas for which conjunctions have not been made
;;; (a subset of the set of all conjuctive schemas, since a conjuctive schema only
;;; gets a conjuction made for it if the conjuction turns out to be reliable).
(defun COMPARE-SCHEMA-PNAMES-AND-EQUALPS (schemas)
  (loop for outer on schemas
	for one = (car outer)
	for one-counter from 0
	do (loop for two in outer		; Yes, this includes comparing each schema with itself.
		 for two-counter from 0
		 do (ignore one-counter two-counter)
		    (break-if-schema-pname-equal-and-my-equalp-differ one two))))

;;; This is a candidate to get turned into a macro called WHEN-SCHEMA-...etc, which just
;;; takes a &body.  Then I can make the BREAK-....etc version just have a body of (BREAK).
;;; This'll happen the very first time I need to use the functionality below without actually
;;; doing a break if they differ (e.g., summarizing their differences, for example).
(defun BREAK-IF-SCHEMA-PNAME-EQUAL-AND-MY-EQUALP-DIFFER (one two)
  (let ((names-equal
	  (equal (schema-print-name one)
		 (schema-print-name two)))
	(my-equalp
	  (my-equalp one two)))
    (unless (eq names-equal my-equalp)
      (break))))

(defun CROSS-PRODUCT-VOID (fn one two)
  ;; Applies FN to the complete cross product of ONE and TWO, discarding the result
  ;; (hence FN must have some side-effect for this to be useful for anything).
  ;; FN will be called as (FN ONE TWO).  [Yes, I know I could make this arbitrarily
  ;; more general, but I don't need that right now...]
  (loop for a in one
	do (loop for b in two
		 do (funcall fn a b))))

;;; This is trying to answer the question, "Is comparing schema print-names the best way to compare
;;; them for equality?"  If the answer to this is "yes", then schema-equalp becomes extremely simple
;;; (and fast!).  So far, the answer is "no" if we're checking reifiers.  With reifiers unchecked, the jury
;;; is still out.  (If the final answer turns out to be "yes", then it should also be trivial to come up with
;;; a simple partial ordering for schemas, so I can sort the schemas generated in any given runs and
;;; compare them more easily, to see if the differences are just random (and would even out with an
;;; infinite-length run), or if there are systematic differences.)
(defun COMPARE-SCHEMA-PNAMES-AND-EQUALPS-IN-SNAPSHOTS (&optional (combos '((0 1) (1 2) (0 2))))
  (loop for (one two) in combos
	do (with-my-equalp-aux-bindings (one two schemas1 schemas2)
	     (cross-product-void
	       #'break-if-schema-pname-equal-and-my-equalp-differ
	       schemas1 schemas2))))

;;;; Sorting (by pname only, at the moment).

(defun SORT-SCHEMAS (schemas &key dont-copy)
  ;; Sort is destructive!  Pay a little in storage by default to keep me from
  ;; screwing up some of the time.
  (sort (if dont-copy
	    schemas				; Freshly-consed
	    (copy-list schemas))		; Not freshly-consed; don't destroy it.
	#'string-lessp
	:key #'schema-print-name))

(defun SHOW-SORTED-SCHEMAS-IN-SNAPSHOT (snapshot-number &key (stream t) (number-lines t) (filter #'true))
  (let* ((schemas (snapshot-spec->schemas-as-list snapshot-number))	; Freshly-consed, so it's okay to destroy this list.
	 (filtered (remove-if-not filter schemas)))	; [In Genera, REMOVE on a freshly-consed, cdr-coded list is (slightly) _faster_ than DELETE!]
    (show-schemas-list
      (sort-schemas filtered :dont-copy t)	; The list is still freshly-consed, whether we used REMOVE or DELETE above...
      :stream stream
      :number-lines number-lines))
  (values))

(defun SHOW-SORTED-SCHEMAS-IN-ALL-SNAPSHOTS (&key (start 0)
					     (end (number-of-snapshotted-world-states))
					     specific-numbers
					     (stream t)
					     (number-lines t)
					     (filter #'true))
  (assert (or (null specific-numbers)
	      (every #'(lambda (elt)
			 (integerp elt)
			 (<= 0 elt (number-of-snapshotted-world-states)))
		     specific-numbers)))
  (let ((numbers
	  (or specific-numbers
	      (loop for number from start below end
		    collect number))))
    (loop for number in numbers
	  do (summarize-snapshots :from number :end (1+ number) :stream stream))
    (format stream "~2&")
    (loop for number in numbers
	  for counter from 0
	  do (unless (zerop counter)
	       (format stream "~2&----------------------------------------------------------------------------------------------------~2&"))
	     (format stream "~&Snapshot ~D:~2&" number)
	     (show-sorted-schemas-in-snapshot
	       number
	       :stream stream
	       :number-lines number-lines
	       :filter filter)))
  (values))

;;;; Exact comparision, the easy way (pnames only, not SCHEMA-EQUALP/MY-EQUALP).

;;; Specifying EXACT-ORDER is not recommended unless you want to see whether
;;; they were all generated in the SAME order...
(defun SNAPSHOT-SCHEMAS-EQUAL? (snap-spec1 snap-spec2 &optional exact-order)
  (let ((one (snapshot-spec->schemas-as-list snap-spec1))
	(two (snapshot-spec->schemas-as-list snap-spec2)))
    (unless exact-order
      (setf one (sort-schemas one :dont-copy t))	; Freshly-consed, hence safe to destroy.
      (setf two (sort-schemas two :dont-copy t)))      
    (cond ((= (length one) (length two))
	   (let ((mismatch (mismatch one two
				     :test #'string-equal
				     :key #'schema-print-name)))
	     (cond (mismatch
		    (format t "~&Mismatch at ~D:~&~A:  ~A~&~A:  ~A~&"
			    mismatch
			    snap-spec1 (nth mismatch one)
			    snap-spec2 (nth mismatch two)))
		   (t
		    (format t "~&No differences encountered.~&")))))
	  (t
	   (format t "~&~A has ~D schema~:P, but ~A has ~D schema~:P.~&"
		   snap-spec1 (length one)
		   snap-spec2 (length two)))))
  (values))

;;;; Doing n-way comparisons.

;;; Note that SSNWC is an abbreviation for SORTED-SCHEMAS-N-WAY-COMPARISON.

;;; Get all the pathnames.
(defun SSNWC-GET-PATHNAMES (pathnames wild-spec)
  ;; Error check.
  (assert (and (not (and pathnames wild-spec))	; Must specify one or the other, but not both.
	       (or pathnames wild-spec)))
  (unless pathnames
    (setf pathnames (directory wild-spec)))
  pathnames)

;;; Computing terse pathname names:  Tokenization.  Simple & stupid.

(defun NAME-NEXT-SEPARATOR (name start)
  (position-if-not #'(lambda (char)
		       (or (char-not-lessp #\Z char #\A)
			   (char-not-lessp #\9 char #\0)))
		   name
		   :start start))

(defun NAME->WORDS (name)
  (let ((tokens nil)
	(done? nil))
    (loop for index from 0 below (length name)
	  until done?
	  do (let ((end (name-next-separator name index)))
	       (cond (end
		      (push (subseq name index end) tokens)
		      (setf index end))
		     (t
		      (push (subseq name index) tokens)
		      (setf done? t)))))
    (nreverse tokens)))

;;; Computing terse pathname names:  Actually making the terse names.

(defun SSNWC-TERSE-NAMES (pathnames)
  (let ((names (mapcar #'(lambda (pathname)
			   (name->words
			     (pathname-name pathname)))
		       pathnames)))
    (setf names (append names (list (car names))))	; Wrap it around to make the logic below simpler.
    (let ((terse-names
	    (loop for one in names
		  for two in (cdr names)
		  while two
		  collect (subseq one
				  (mismatch one two :test #'string-equal)
				  (mismatch one two :test #'string-equal :from-end t)))))
      (mapcar #'(lambda (tokenization)
		  (string-capitalize
		    (reduce #'(lambda (string1 string2)
				(string-append string1 "-" string2))
			    tokenization)))
	      terse-names))))

;;; Annotates each terse name with the filters applied to it, suppressing #'TRUE.
;;;
;;; The annotations are in composition order (e.g., since a filter in FILTERS is applied
;;; before the filter in FILTER, if the former is '(A) and the latter is 'B, we'll append
;;; "B A" to the name (similar to the way you'd write it in lisp, e.g., "(B (A thing))",
;;; hence "composition order").  [Not that all this rigorous hair is likely to be all that
;;; important, since I'd presume that composition order of filters is irrelevant, but...]
;;;
;;; This has the interesting bug that, if FILTERS is something like '(true unreliable-schemas reliable-schemas),
;;; [instead of (list #'true ... etc)], we wind up comparing 'true with #'true, failing, and annotating " #'true"
;;; for the first one.  This is a likely outcome of the "easy" way of calling the function below that compares
;;; a single snapshot run through different filters.  I'm not going to fix this bug (which would be trivial),
;;; because I happen to _like_ the fact that this gives a nice, parallel construction to the headings.
(defun SSNWC-FILTERING-ANNOTATE (terse-names global-filter filters)
  (assert (= (length terse-names) (length filters)))
  (labels ((filter-name (filter)
	     (if (eq filter #'true)
		 ""
		 (format nil " ~(#'~A~)" filter)))
	   (annotate-1 (name filter)		; Done in this screwy semi-recursive way to get one, ...
	     (string-append			; ... and only one, space between a pair of filters.
	       (format nil "~A" name)		; In case it's an integer or something...
	       (filter-name filter)))
	   (annotate (name filter)
	     (annotate-1 (annotate-1 name filter) global-filter)))
    (mapcar #'annotate terse-names filters)))

;;; Suck in all the lines, tagged with the number of their stream.
(defun SSNWC-SUCK-IN-LINES (pathnames)
  ;; Open all the streams.
  (let ((open-streams nil))
    (unwind-protect
	(progn
	  (loop for pathname in (reverse pathnames)	; So the result matches the order in the filesystem ...
		do (push (open pathname) open-streams))	; ... despite using PUSH here.
	  ;; Suck all the lines into an array, tagged with their stream.
	  (let ((all-lines (make-array (length open-streams))))
	    (loop with done?
		  until done?
		  do (let ((lines-from-streams
			     (loop for stream in open-streams
				   for line = (read-line stream nil)
				   collect line)))
		       (cond ((every #'null lines-from-streams)
			      (setf done? t))
			     (t
			      (loop for line in lines-from-streams
				    for index from 0
				    when line	; Don't bother pushing NILs after we've run off the end of this stream.
				      do (push (cons line index)
					       (aref all-lines index)))))))
	    ;; Reverse the lines in the array, so they come out in the order they were read.
	    (loop for index from 0 below (length all-lines)
		  do (setf (aref all-lines index)
			   (reverse (aref all-lines index))))
	    all-lines))				; Return the array of tagged lines.
      ;; Close all streams.
      (loop for stream in open-streams
	    when stream
	      do (close stream)))))

(defun SSNWC-MERGE (data)
  (let ((result (aref data 0)))
    (loop for index from 1 below (length data)
	  do (setf result (merge 'list
				 result (aref data index)
				 #'string-lessp
				 :key #'car)))
    result))

(defun SSNWC-SORT (data)
  (sort (reduce #'append data) #'string-lessp :key #'car))

(defun SORTED-SCHEMAS-FROM-SOURCE (sorted-schemas n)
  (mapcar #'car					; Strip off the tag.
	  (remove-if-not #'(lambda (tag)
			     (= tag n))
			 sorted-schemas
			 :key #'cdr)))

;;; Requires a presorted list of schema printed-reps.
;;; Might itself emit duplicates if something is, e.g, triplicated.
;;; If N in supplied, assumes it has to do the selection from a source itself.
(defun SORTED-SCHEMAS-DUPLICATES (sorted-schemas &optional n)
  (when n
    (setf sorted-schemas (sorted-schemas-from-source sorted-schemas n)))
  (loop for one in sorted-schemas
	while one				; Just in case some wiseguy handles us a singleton list.
	for two in (cdr sorted-schemas)
	while two				; Terminate when we're out of pairs.
	when (equalp one two)			; What the hell, allow using this for non-strings.
	  collect one))

(defun SHOW-SORTED-SCHEMAS-DUPLICATES (sorted-schemas &optional n (stream t))
  (let ((duplicates (sorted-schemas-duplicates sorted-schemas n)))
    (loop for dup in duplicates
	  do (format stream "~&~A~&" dup)))
  (values))

(defun MAKE-PASSTHROUGH-FILTERS (n)
  (make-list n :initial-element #'true))

;;; Since I can't seem to decide how to set this, ever...
;;; Note that using a dash instead of an underscore makes it difficult to tell a
;;; negated schema from the aligner character!
(defparameter *SSNWC-ALIGNER* #\_)

;;; Note that this goes ahead and shows the main output even in the face of
;;; duplicates.  I can't decide if this is a bug or a feature, but I don't feel like adding
;;; yet another keyword at the moment.
(defun SSNWC-OUTPUT (pathnames ordered stream width &key filter filters
		     (check-duplicates t)	; Check for duplicates at all.
		     (show-duplicates t)	; If there are any, show them.  Only relevant if CHECK-DUPLICATES is non-NIL.
		     (duplicates-only nil)	; Don't print the "normal" output; print only the duplicates, then stop.
		     (preestablished-max-width))	; Only from a self-recursive call.
  ;; Initialize & error check.
  (assert (= (length pathnames) (length filters)))	; NOT the same as (LENGTH ORDERED)!
  (let* ((unannotated-terse-names
	   (if (pathnamep (car pathnames))	; If the first one's a pathname, ...
	       (ssnwc-terse-names pathnames)	; ... then assume that they all are, and terse-ify them.
	       pathnames))			; Otherwise, assume that they're the correct column headings already.
	 (terse-names
	   (ssnwc-filtering-annotate
	     unannotated-terse-names filter filters)))
    (let ((max-width (or preestablished-max-width
			 (1+ (max (longest-printed-rep ordered #'car)	; Column width must be the max of the headings and the data.
				  (longest-printed-rep terse-names #'identity)))))
	  (columns (length pathnames)))
      (when (> (* max-width columns) width)
	(cerror "Print it anyway" "~D columns of ~D characters each won't fit in a total width of ~D."
		columns
		max-width
		width))
      (when check-duplicates
	(let ((dups
		(loop for i from 0 below columns
		      collect (sorted-schemas-duplicates ordered i))))
	  (unless (every #'null dups)
	    (cond (show-duplicates
		   (let* ((dups-as-lines	; An array of lists of (line . tag) entries.
			    (ssnwc-schema-sources-to-sucked-in-lines dups))
			  (sorted-dups		; A single, sorted list of (line . tag) entries.
			    (ssnwc-sort dups-as-lines))
			  (null-filters		; Any filter annotations have already been made in the outer call, so don't do it again!
			    (make-passthrough-filters columns)))
		     (bold-italic-format stream "~&Duplicate schema~P (~:*~D total):~2&" (length sorted-dups))
		     (ssnwc-output		; Okay to check for recursive duplicates on recursive call, I guess...
		       terse-names sorted-dups stream width    ; Don't need to propagate :CHECK-DUPLICATES---we can't be here if it was NIL.
		       :show-duplicates show-duplicates	; However, we _do_ need to propagate this---maybe the user only wanted a summary.
		       :filter  #'true		; To avoid a spurious #'nil after each pathname.
		       :filters null-filters	; So the assertion about filters & pathnames wins.
		       :preestablished-max-width max-width))	; So our output columns will line up with the main ones.
		   (unless duplicates-only
		     (bold-italic-format stream "~2&All schemas, duplicates suppressed:~2&")))
		  (t
		   (bold-italic-format
		     *error-output* "~&Warning:  Duplicates!  One-to-one with pathnames, they number:  ~S~&"
		     (mapcar #'length dups)))))))
      (unless duplicates-only			; Recursive calls never propagate a non-NIL value of this, so the recursive calls will work okay.
	(ssnwc-output-internal
	  stream ordered terse-names columns max-width))))
  (values))

(defun SSNWC-OUTPUT-INTERNAL (stream ordered terse-names columns max-width)
  (let ((aligner (make-string (* columns max-width))))
    (flet ((output (thing column)
	     (format stream "~:[~VT~;~*~]~A"	; &&& Was SAFE-FORMAT.  [!SF!]
		     (zerop column)		; Compensate for what I consider a bug (dunno if Genera-specific):  ~VT for V=0 still prints a single space!
		     (* column max-width)
		     thing))
	   (stuff (thing column)
	     (replace aligner thing :start1 (* column max-width))))
      ;; Print column headings.
      (format stream "~&")
      (loop for name in terse-names
	    for column from 0
	    do (output (bold-format nil "~A" name) column))
      (terpri)
      ;; Print data.
      (loop with last-printed = ""
	    for (item . tag) in ordered
	    unless (string-equal item last-printed)	; If this item isn't the same as the last one, then print it on a different line.
	      do (unless (zerop (length last-printed))	; Don't print a line of nulls before the very first item.
		   (format stream "~A~&" aligner))
		 (fill aligner *ssnwc-aligner*)
	    do (setf last-printed item)
	       (stuff item tag)))
    (format stream "~A~&" aligner)))

;;; This is a toplevel.
(defun SSNWC (&key
	      pathnames				; E.g., '("path1" "path2" ...)
	      wild-spec				; E.g., "EQ:>Foner>Schema>Runs>Old>Attention-Macsyma>*sorted*.terse.newest"
	      (stream t)
	      (input-already-sorted t)
	      (width (default-table-width))
	      (check-duplicates t)		; Check for duplicates at all.
	      (show-duplicates t)		; If there are any, show them.  Only relevant if CHECK-DUPLICATES is non-NIL.
	      (duplicates-only nil))		; Don't print the "normal" output; print only the duplicates, then stop.
  (let* ((pathnames
	   (ssnwc-get-pathnames pathnames wild-spec))
	 (data
	   (ssnwc-suck-in-lines pathnames))
	 (ordered
	   (if input-already-sorted
	       (ssnwc-merge data)		; Much more efficient, but requires already-in-order input.
	       (ssnwc-sort  data)))		; Less efficient, but more robust.
	 (null-filters				; See below.
	   (make-passthrough-filters (length pathnames))))
    (ssnwc-output
      pathnames ordered stream width
      :filter  #'true				; To avoid a spurious #'nil after each pathname.
      :filters null-filters			; So the assertion about filters & pathnames wins.
      :check-duplicates check-duplicates
      :show-duplicates  show-duplicates
      :duplicates-only  duplicates-only))
  (values))

;;; Suck in all the schemas from their snapshots, tagged with the number of their stream.
;;; Stolen from SSNWC-SUCK-IN-LINES.
;;; Note that this version, like that version, allows the number of schemas to differ
;;; (the snapshots don't even have to have the same *SCHEMA-MAXIMUM* linit).
;;; Unlike that version, this version allows filtering based on some criterion
;;; (presumably RELIABLE-SCHEMA? or something like it):  FILTER filters everybody,
;;; while FILTERS, if supplied, should be a list mapping one-to-one to the specs.
;;; A filter in FILTERS is applied before the filter in FILTER, if that matters...
;;;
;;; Work function, to allow the next one to be used more generally elsewhere.
(defun SSNWC-SNAPSHOTS-TO-FILTERED-SCHEMAS (snapshot-specs &key
					    (filter #'true)
					    (filters (make-passthrough-filters (length snapshot-specs))))
  ;; Find all the snapshots.
  (let ((schema-sources nil)
	(number-of-sources (length snapshot-specs)))
    (assert (= number-of-sources (length filters)))
    (flet ((quick-filter (filter what)
	     (if (eq filter #'true)
		 what				; Might as well do the easy case quickly and without consing another sequence.
		 (remove-if-not			; NOTE that this cannot be DELETE!  Otherwise we'll smash the snapshotted schema vector.
		   filter what))))
      (loop for spec in (reverse snapshot-specs)	; So the result matches the order in the args ...
	    for one-filter in (reverse filters)
	    for schemas  = (snapshot-spec->schemas-as-list spec)
	    for filtered = (quick-filter filter (quick-filter one-filter schemas))
	    do (push filtered schema-sources)))
    schema-sources))

;;; Actually generate the "sucked in lines", using data generated by the above routine.
(defun SSNWC-SCHEMA-SOURCES-TO-SUCKED-IN-LINES (schema-sources)
  ;; Find all the snapshots.
  (let ((number-of-sources (length schema-sources)))
    (let ((all-lines (make-array number-of-sources))	; Stores "lines" from the various snapshots.
	  (all-locfs (make-array number-of-sources)))	; Stores the current position in each list of schemas (one list per snapshot).
      (loop for source in schema-sources	; Initialize where we are in each source's list of schemas.
	    for index from 0			; [Note that these aren't really locatives, which are Genera-specific:  ...
	    do (setf (aref all-locfs index) source))	; ... they're just ordinary list elements.]
      (loop with done?
	    until done?
	    do (let ((schemas-from-sources	; Using trailing list pointer enumeration, tra la!
		       (loop for i from 0 below number-of-sources
			     collect (car (aref all-locfs i))
			     do (setf (aref all-locfs i)
				      (cdr (aref all-locfs i))))))	; Fortunately, CDR of NIL is NIL, not an error.
		 (cond ((every #'null schemas-from-sources)
			(setf done? t))		; Must have run off all of them, if all CARs are NIL.
		       (t
			(loop for schema in schemas-from-sources
			      for index from 0
			      when schema
				do (push (cons (if (schema-p schema)	; Allow using a literal string here, from a recursive call to the output function.
						   (schema-print-name schema)
						   schema)
					       index)
					 (aref all-lines index)))))))
      ;; Reverse the lines in the array, so they come out in the order they were read.
      (loop for index from 0 below (length all-lines)
	    do (setf (aref all-lines index)
		     (reverse (aref all-lines index))))
      all-lines)))				; Return the array of tagged lines.

;;; This is a toplevel.
(defun SSNWC-FROM-SNAPSHOTS (snapshot-specs &key
			     (stream t)
			     (width (default-table-width))
			     (filter #'true)
			     (filters (make-passthrough-filters (length snapshot-specs)))
			     (check-duplicates t)	; Check for duplicates at all.
			     (show-duplicates t)	; If there are any, show them.  Only relevant if CHECK-DUPLICATES is non-NIL.
			     (duplicates-only nil))	; Don't print the "normal" output; print only the duplicates, then stop.
  (let* ((schema-sources
	   (ssnwc-snapshots-to-filtered-schemas
	     snapshot-specs
	     :filter  filter
	     :filters filters))
	 (data
	   (ssnwc-schema-sources-to-sucked-in-lines
	     schema-sources))
	 (ordered
	   (ssnwc-sort data)))			; The data is never presorted in this case (even if the filter is #'TRUE).
    (ssnwc-output
      snapshot-specs ordered stream width
      :filter  filter
      :filters filters
      :check-duplicates check-duplicates
      :show-duplicates  show-duplicates
      :duplicates-only  duplicates-only))
  (values))

;;; This is a toplevel.
;;;
;;; This is a convenience function for calling the above for a _single_ snapshot,
;;; filtered in n different ways.  It's perfectly possible to call it directly (in the
;;; manner we're doing here), but this is easier for that case.
(defun SSNWC-FROM-SNAPSHOT-FILTERED (snapshot-spec filters &key
				     (stream t)
				     (filter #'true)
				     (width (default-table-width))
				     (check-duplicates t)	; Check for duplicates at all.
				     (show-duplicates t)	; If there are any, show them.  Only relevant if CHECK-DUPLICATES is non-NIL.
				     (duplicates-only nil))	; Don't print the "normal" output; print only the duplicates, then stop.
  (assert (not (null filters)))			; NIL just won't cut it here.
  (ssnwc-from-snapshots
    (make-list (length filters) :initial-element snapshot-spec)
    :filter  filter
    :filters filters
    :stream  stream
    :width   width
    :check-duplicates check-duplicates
    :show-duplicates  show-duplicates
    :duplicates-only  duplicates-only))

;;; This is a toplevel.
;;;
;;; And this is yet another convenience function, for comparing the reliable and
;;; unreliable schemas of a single snapshot.
(defun SSNWC-RELIABLE-VS-UNRELIABLE (snapshot-spec &key
				     (stream t)
				     (filter #'true)
				     (width (default-table-width))
				     (check-duplicates t)	; Check for duplicates at all.
				     (show-duplicates t)	; If there are any, show them.  Only relevant if CHECK-DUPLICATES is non-NIL.
				     (duplicates-only nil))	; Don't print the "normal" output; print only the duplicates, then stop.
  (ssnwc-from-snapshot-filtered
    snapshot-spec
    (list #'true #'unreliable-schema? #'reliable-schema?)
    :filter  filter
    :stream  stream
    :width   width
    :check-duplicates check-duplicates
    :show-duplicates  show-duplicates
    :duplicates-only  duplicates-only))

;;; Yet another convenience.
;;;
;;; %%% Note that this is peculiarly written, because we try to portably print the
;;; name of the filter.  Using FLET creates an atrocity which is too long to print, so
;;; we punt and make a short little routine that looks in a special variable (yuck!).  I
;;; suppose I should rewrite the inner filtering function to pass optional args to the
;;; filter or something...  But that's a fair amount of mechanical code rewriting that I
;;; just don't want to do at the moment.
(defvar *SMS* nil)

(defun SMS (schema)
  (assert (not (null *sms*)))
  (loop for substring in *sms*
	always (search substring (schema-print-name schema))))
  
(defun SSNWC-SCHEMAS-MENTIONING-SUBSTRINGS (substrings snapshot-spec &key
					    (stream t)
					    (width (default-table-width))
					    (check-duplicates t)	; Check for duplicates at all.
					    (show-duplicates t)	; If there are any, show them.  Only relevant if CHECK-DUPLICATES is non-NIL.
					    (duplicates-only nil))	; Don't print the "normal" output; print only the duplicates, then stop.
  (let ((*sms* (if (consp substrings)
		   substrings
		   (list substrings))))
    (ssnwc-reliable-vs-unreliable
      snapshot-spec
      :filter  #'sms
      :stream  stream
      :width   width
      :check-duplicates check-duplicates
      :show-duplicates  show-duplicates
      :duplicates-only  duplicates-only)))

;;;; Saving bunches.

(defun BUNCH-SET-RELATIONS-SIZE (&optional (relations *bunch-set-relations*))
  (reduce #'+					; Add up all the lengths (3 combos by default).
	  (mapcar				; Get the total length of all three of (intersection, diff12, diff21) for some comparison.
	    #'(lambda (inside)
		(reduce #'+			; Sum the number of schemas in a particular triple of (intersection, diff12, diff21).
			(mapcar			; Get the length of some intersection or difference.
			  #'length inside)))
	    relations)))

;;; Special-purpose hack.
#+Genera
(defun SAVE-BUNCH-SET-RELATIONS (&optional (where "FEP1:>Comparisons>bunch-set-relations.ibin"))
  (dumping-schemas ((bunch-set-relations-size *bunch-set-relations*))
    (dump-variables where *bunch-set-relations*)))

;;; Special-purpose hack.  [This came first, before the BUNCH-SET-RELATIONS stuff.]
(defun SAVE-SET-RELATIONS (&optional (where "FEP1:>Comparisons>set-relations.ibin"))
  (declare (special intersection one-two two-one
		    intersection-1 one-two-1 two-one-1
		    intersection-2 one-two-2 two-one-2))
  (dump-variables where
		  intersection			; 10 11.
		  one-two
		  two-one
		  intersection-1		; 11 12.
		  one-two-1
		  two-one-1
		  intersection-2		; 10 12.
		  one-two-2
		  two-one-2))

;;; Turn the old stuff into a bunch, but keep it separate from the other bunch...
(defvar *OLD-BUNCH-SET-RELATIONS* nil)

(defun BUNCHIFY-OLD-VARIABLES ()
  (declare (special intersection one-two two-one
		    intersection-1 one-two-1 two-one-1
		    intersection-2 one-two-2 two-one-2))
  (setf *old-bunch-set-relations*
	(list (list intersection   one-two   two-one)
	      (list intersection-1 one-two-1 two-one-1)
	      (list intersection-2 one-two-2 two-one-2))))

#||
(summarize-snapshots)				; This was what SAVE-BUNCH-SET-RELATIONS saved.
 0   10763  "7/05/93 00:01:57"  (6 11)  "Bunch run to 10763."
 1   10181  "7/04/93 12:53:58"  (6 10)  "Bunch run to 10181."
 2    9839  "7/03/93 20:44:15"  (6 9)  "Bunch run to 9839."
||#

#||
(summarize-snapshots)				; This was what SAVE-SET-RELATIONS saved...
 0    1000  "7/04/93 14:45:20"  (6 11)  "Bunch run to 1000."
 1    2000  "7/04/93 14:51:55"  (6 11)  "Bunch run to 2000."
 2    3000  "7/04/93 15:03:45"  (6 11)  "Bunch run to 3000."
 3    4000  "7/04/93 15:21:50"  (6 11)  "Bunch run to 4000."
 4    5000  "7/04/93 15:46:11"  (6 11)  "Bunch run to 5000."
 5    6000  "7/04/93 16:20:20"  (6 11)  "Bunch run to 6000."
 6    7000  "7/04/93 17:08:40"  (6 11)  "Bunch run to 7000."
 7    8000  "7/04/93 18:14:42"  (6 11)  "Bunch run to 8000."
 8    9000  "7/04/93 19:45:55"  (6 11)  "Bunch run to 9000."
 9   10000  "7/04/93 21:50:02"  (6 11)  "Bunch run to 10000."
10   10763  "7/05/93 00:01:57"  (6 11)  "Bunch run to 10763."
11   10181  "7/04/93 12:53:58"  (6 10)  "Bunch run to 10181."
12    9839  "7/03/93 20:44:15"  (6 9)  "Bunch run to 9839."
13    9839  "7/03/93 20:44:15"  (6 9)  "Bunch run to 9839."
||#

;;; End of file.
