;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Iterators used in the new spinoff schema function below.

(in-package :schema)

;;; These are a variety of experimental iterators, to try different selections of
;;; control flow.  The general idea is that a specialized generator is defined that
;;; maps into some sequence of numbers (array indices for our data structures,
;;; really---pity this wasn't originally implemented in a less C-like fashion), which
;;; keeps returning "appropriate" numbers until exhausted, at which point it returns
;;; NIL.  By swapping which generator is used, different schemas/items are used
;;; without excessively editing the source of MAYBE-SPINOFF-SCHEMA.  Each iterator
;;; has associated with it a "resetter", which reinitializes the iterator so that it
;;; behaves as if it was just created.  This is a consing efficiency hack (since
;;; downward-function/funarg declarations don't seem to be working as expected in
;;; Genera and may not be available in MCL anyway), as it means that we can cons a
;;; closure once (upon entry to MAYBE-SPINOFF-SCHEMA), and then keep reusing it
;;; inside inner loops.

;;;; General-purpose control flow, for use with iterators.

;;; Just creates the yielder (iterator) and resetter from some generator.
;;; Both must be funcalled from with the body.
(defmacro WITH-ITERATOR-FUNCTIONS ((yielder resetter generator) &body body)
  `(multiple-value-bind (,yielder ,resetter)
       (funcall ,generator)
     ,@body))

;;; Just creates the yielder (iterator) and resetter from some generator.
;;; Both may be used as ordinary symbols naming functions (applyable).
;;; (I could get _really_ funky and use symbol-macros, but let's not...)
(defmacro WITH-ITERATOR ((yielder resetter generator) &body body)
  (let ((y (gensymbol "YIELDER-"))
	(r (gensymbol "RESETTER-")))
    `(multiple-value-bind (,y ,r)
	 (funcall ,generator)
       (macrolet ((,yielder ()
		   `(funcall ,',y))
		  (,resetter ()
		   `(funcall ,',r)))
	 ,@body))))

;;; Takes the iterator and resetter and acts like DO or DOTIMES with them.
;;; Expects them to be passed in because DO-ITERATOR might be getting called
;;; inside an inner loop, and whatever is outside it is responsible for consing
;;; up the iterator/resetter just once.  They must be applyable, not funcallable
;;; (e.g., from WITH-ITERATOR, not WITH-ITERATOR-FUNCTIONS).
(defmacro DO-ITERATOR ((yielder resetter yielded-value &optional resultform) &body body)
  `(do ((,yielded-value
	 (progn (,resetter) (,yielder))
	 (,yielder)))
       ((null ,yielded-value) ,resultform)
     ,@body))

;;;; Conveniently using different iterators in blocks of code.

;;; Things here get summarized in the log every time we start a major iteration or bunch.
;;; We push tuples of (LABEL, SCHEMA-ITERATOR-GENERATOR, ITEM-ITERATOR-GENERATOR) onto this.
;;; LABEL shound be a pretty name indicating which module we're talking about; we'll capitalize
;;; the first letter if necessary.
(defvar *ITERATOR-GENERATORS-IN-USE* nil)

;;; Not the greatest name here...
(defmacro-definer DEF-ITERATOR-GENERATOR (module default-schema-generator default-item-generator)
  #+Genera (declare (zwei:indentation 1 1))
  (flet ((make-variable (module type)
	   (intern-format "*use-which-~A-~A-number-generator?*" module type))
	 (make-macro (module type)
	   (intern-format "with-~A-~A-number-generator" module type)))
    (let ((schema-variable (make-variable module "schema"))
	  (item-variable   (make-variable module "item"))
	  (schema-macro    (make-macro module "schema"))
	  (item-macro      (make-macro module "item")))
      `(progn
	 ;; Note that these use ' and not #' so that we pick up changes to the definitions!
	 (defparameter ,schema-variable ',default-schema-generator)
	 (defparameter ,item-variable   ',default-item-generator)
	 ;; The use of COMPILER-LET below is to allow code compiled lexically inside one of
	 ;; these to use the current bindings of the *USE-WHICH-n-m-NUMBER-GENERATOR?*,
	 ;; so we can make composited functions at compile-time, as is done in GOALS.LISP.
	 (defmacro ,schema-macro (generator &body body)
	   `(let ((,',schema-variable ,generator))
	      (compiler-let ((,',schema-variable ,generator))
		,@body)))
	 (defmacro ,item-macro (generator &body body)
	   `(let ((,',item-variable ,generator))
	      (compiler-let ((,',item-variable ,generator))
		,@body)))
	 (pushnew-car-replace (list ,(symbol-name module) ',schema-variable ',item-variable)
			      *iterator-generators-in-use*
		  :test #'string-equal)
	 ))))

(defun ITERATION-GENERATORS-IN-USE-PARAMETRIC-SNAPSHOT ()
  (loop for (module schema-variable item-variable) in *iterator-generators-in-use*
	collect (list module
		      (list schema-variable (symbol-value schema-variable))
		      (list item-variable   (symbol-value item-variable)))))

;;;; Iterators.

;;; NOTE:  It is NOT necessary to call RESET before the first call to YIELD, nor
;;; will it hurt to do so anyway, for ANY iterator (by definition).  Note that
;;; complex iterators which sort lists or whatever are not required to sort
;;; in a different order if you call RESET again; if you want to pick up changes
;;; to the sequence, you must make a new iterator.  DO-ITERATOR happens to
;;; call the resetter before first calling the yielder anyway, but there's no
;;; guarantee, I suppose, that we're using DO-ITERATOR at any given point
;;; (I haven't mandated that WITH-ITERATOR use DO-ITERATOR internally).

;;;; +++ Simplest form of iterator:  Just returns integers from FROM below BELOW.
(defmacro-definer DEF-SEQUENCE-ITERATOR (name below &optional (from 0))
  `(defun ,name ()
;    (declare (sys:downward-function))		; Doesn't take effect anyway.
     (let ((i ,from))
       (values
	 #'(lambda ()				; YIELD.
	     (when (< i ,below)
	       (prog1 i
		      (incf i))))
	 #'(lambda ()				; RESET.
	     (setf i ,from))))))

;;; These each return a function which, when called, returns the iterator and resetter.
(def-sequence-iterator ALL-ITEM-NUMBERS-GENERATOR *item-number*)
(def-sequence-iterator ALL-SCHEMA-NUMBERS-GENERATOR *schema-number*)

;;;; +++ More complicated iterator:  returns integers in some particular order.

;;; We are given an input sequence and a sorting predicate.
;;; We return a new sequence which is a permutation vector:  were we to
;;; access each element of the original sequence in the order indicated by
;;; our returned sequence, we would access the original sequence in the order
;;; specified by the sorting predicate.

;;; Utility function.
;;; [For now, this is an inefficient implementation!  A more efficient one would
;;; not cons, and would keep the last sequence around and not sort it again if
;;; it's the same sequence in the same order.]
;;; &&& It's _really_ a pain that so much of the rest of the schema code wants indices,
;;; not actual objects.  It means I have to keep indices around redundantly in calls,
;;; such as the input-indices arg below...  *sigh*
(defun MAKE-PERMUTATION-SEQUENCE (predicate input-indices input-active-length)
  ;; We return a vector, not a list.  INPUT-ACTIVE-LENGTH is equivalent to an array
  ;; with a leader, except of course that all of this stuff was using globals to hold
  ;; array lengths instead of using leaders, which is what they're for.  *sigh*
  ;; INPUT-INDICES is a list of those indices into whatever array the predicate accesses.
  ;; The predicate is responsible for turning some integer into a reference to
  ;; something it can compare to something else (e.g., the predicate gets two
  ;; indices and should compare what they point at).
  (let* ((temp nil)
	 (output (make-array input-active-length :element-type 'fixnum)))
    (loop for i from 0 below input-active-length
	  do (setf temp (acons (aref input-indices i) i temp)))
    ;; If the input was already completely or mostly in order, make sure that our temporary
    ;; is, too.  Otherwise, we risk calling sort on an exactly-reversed-order sequence, which
    ;; tends to be worse-case for many sorting algorithms (e.g., O(n^2) instead of O(log n)).
    (setf temp (nreverse temp))
    (setf temp (sort temp predicate :key #'car))
    (loop for i from 0 below input-active-length
	  for elt in temp
	  do (setf (aref output i) (car elt)))
    output))

;;; The most common idiom:  Take a vector and walk down its length.
(defmacro MAKE-SIMPLE-YIELDER-AND-RESETTER (vector &optional below (from 0))
  `(let ((i ,from)
	 (limit ,(or below
		     `(length ,vector))))
     (values
       #'(lambda ()				; YIELD.
	   (when (< i limit)
	     (prog1 (aref ,vector i)
		    (incf i))))
       #'(lambda ()				; RESET.
	   (setf i ,from)))))

(defmacro-definer DEF-SORTED-SEQUENCE-ITERATOR (name predicate input-indices input-active-length)
  `(defun ,name ()
     (let ((permutation (make-permutation-sequence ,predicate ,input-indices ,input-active-length)))
       (make-simple-yielder-and-resetter permutation))))

(defsubst ITEM-NUMBER-GENERALITY-< (one two)
  (item-generality-<
    (get-item one)
    (get-item two)))

(defsubst ALL-ACTIVE-ITEM-NUMBERS-IN-ORDER ()
  (make-array *item-number*
	      :element-type 'fixnum
	      :initial-contents (loop for counter from 0 below *item-number*
				      collect counter)))

(def-sorted-sequence-iterator ALL-ITEM-NUMBERS-MOST-SPECIFIC-FIRST-GENERATOR
			      #'item-number-generality-<
  (all-active-item-numbers-in-order)
  *item-number*)

;;;; Items which changed state, and schemas which depend on particular items,
;;;; for use by iterators that return them.

(defsubst ITEM-NUMBERS-TO-ITEMS (numbers)
  (mapcar #'(lambda (thing)			; *sigh*  Another one that should have been a subst, not a macro...
	      (get-item thing))
	  numbers))

(defun ITEM-NUMBERS-WHICH-CHANGED-STATE ()
  (loop for item-index from 0 below *item-number*
	for item = (get-item item-index)
	while item
	unless (eql (item-current-state item) (item-last-state item))
	  collect item-index))

(defsubst ITEMS-WHICH-CHANGED-STATE ()
  (item-numbers-to-items
    (item-numbers-which-changed-state)))

(defsubst NAME-ITEMS-WHICH-CHANGED-STATE ()	; For debugging.
  (mapcar #'item-print-name (items-which-changed-state)))

;;; Hmm.  No callers?  (Schema 11.8.)  Probably 'cause this didn't work out very well.
;;; [This was #+Genera'd on 27 Jan 94 to insulate it from HCL, because it calls
;;;  SCL:BIT-VECTOR-ZERO-P.  I don't want to have to write this unless I have to,
;;;  and, since this isn't called, I guess it means that I don't have to.]
#+Genera
(defun ITEMS-WITH-DEPENDENTS ()
  ;; Returns, in each value, a list of sublists.  Each sublist has an item number as its car,
  ;; and a list of dependent schema numbers as its cdr.
  (loop for i from 0 below *item-number*
	for item = (get-item i)
	for c = (item-context-dependent-schemas item)
	for r = (item-result-dependent-schemas item)
	unless (scl:bit-vector-zero-p c)
	  collect (list i
			(loop for j from 0 below *schema-number*
			      unless (zerop (bit c j))
				collect j))
	    into dependent-contexts
	unless (scl:bit-vector-zero-p r)
	  collect (list i
			(loop for j from 0 below *schema-number*
			      unless (zerop (bit r j))
				collect j))
	    into dependent-results
	finally (return (values dependent-contexts
				dependent-results))))

;;; A common idiom.  Binds particularly-named variables.
(defmacro WITH-VECTOR-FROM-LIST-PRODUCER ((producer &rest producer-args) &body body)
  `(let* ((relevant-numbers-as-list
	    (funcall ,producer ,@producer-args))
	  (length
	    (length relevant-numbers-as-list))
	  (relevant-numbers-as-vector
	    (make-array length
			:element-type 'fixnum
			:initial-contents relevant-numbers-as-list)))
     ,@body))

;;; Another common idiom.
(defmacro MAKE-GENERATOR-FROM-LIST-PRODUCER (producer &rest producer-args)
  `(with-vector-from-list-producer (,producer ,@producer-args)
     (make-simple-yielder-and-resetter relevant-numbers-as-vector)))

(defun CHANGED-ITEM-NUMBERS-GENERATOR ()
  (make-generator-from-list-producer
    #'item-numbers-which-changed-state))

(defun CHANGED-ITEM-NUMBERS-MOST-SPECIFIC-FIRST-GENERATOR ()
  ;; This is _almost_ the right thing for DEF-SORTED-SEQUENCE-ITERATOR, except that we have
  ;; to undergo substantial computation to generate the args for that, and I'm trying not to
  ;; have to do it twice, etc...
  (with-vector-from-list-producer (#'item-numbers-which-changed-state)
    (let ((permutation
	    (make-permutation-sequence
	      #'item-number-generality-<
	      relevant-numbers-as-vector
	      length)))
      (make-simple-yielder-and-resetter permutation))))

(defsubst ENUMERATE-BIT-VECTOR (vector)
  (loop for index from 0 below (length vector)
	unless (zerop (bit vector index))
	  collect index))

(defsubst ENUMERATE-VECTOR (vector &optional off)
  (loop for index from 0 below (length vector)
	unless (eql (aref vector index) off)
	  collect index))
  
;;; This is like WITH-VECTOR-FROM-LIST-PRODUCER, but slightly more specialized.
; (defmacro WITH-VECTOR-FROM-BIT-VECTOR (bit-vector &body body)
;   `(with-vector-from-list-producer (#'enumerate-bit-vector ,bit-vector)
;      ,@body))

;;; Faster replacement for the above (doesn't cons the intermediate list).
;;; Note that we must bind LENGTH and RELEVANT-NUMBERS-AS-VECTOR,
;;; since callers expect them.  This runs in 1/2 to 1/3 of the time of the
;;; original, and doesn't cons any list structure at all.
(defmacro WITH-VECTOR-FROM-BIT-VECTOR-1 (bit-vector &body body)
  `(let* ((length
	    (count 1 ,bit-vector))
	  (relevant-numbers-as-vector
	    (make-array length
			:element-type 'fixnum)))
     (loop with v-i = 0
	   for bv-i from 0 below (length ,bit-vector)
	   while (< v-i length)
	   unless (zerop (bit ,bit-vector bv-i))
	     do (setf (aref relevant-numbers-as-vector v-i) bv-i)
		(incf v-i))
     (progn					; PROGN so body declarations can win.
       ,@body)))

(defmacro MAKE-SIMPLE-YIELDER-AND-RESETTER-FROM-BIT-VECTOR (bit-vector)
  `(with-vector-from-bit-vector ,bit-vector
     (make-simple-yielder-and-resetter relevant-numbers-as-vector length)))

;;; Note that this doesn't bother to make its local variables gensym'ed & hence
;;; unreachable to the body.  Note further that part of the contract of this macro
;;; is to make available the variable SCHEMA-BIT-VECTOR, which the SELECTOR modifies.
(defmacro WITH-SOME-SCHEMAS (selector &body body)
  `(let* ((schema-bit-vector
	    (make-schema-bit-vector))
	  (selected-items
	    (item-numbers-to-items
	      (funcall ,selector))))
     ;; Set all the schemas selected.
     (loop for item in selected-items
	   for c = (item-context-dependent-schemas item)
	   for r = (item-result-dependent-schemas item)
	   do (bit-ior schema-bit-vector c t)	; T -> bash SCHEMA-BIT-VECTOR.
	      (bit-ior schema-bit-vector r t))	; Ditto.
     ,@body))

;;; +++
;;; These are like WITH-SOME-SCHEMAS, with a few differences:
;;;   They split up the the two possibilities of context- or result-dependent schemas.
;;;   They take an optional bit-vector as input, to avoid consing by allowing the caller
;;;    to reuse bit-vectors.  (The bit-vector is cleared before we start ORing into it.)
;;;    Note the slightly different arglist required to enable the optional argument.
;;;    [&&& It's possible that I might want to upgrade WITH-SOME-SCHEMAS to also
;;;    allow passing in the bit-vector, if I want improved efficiency somewhere.]
(defmacro WITH-SOME-DEPENDENT-SCHEMAS-INTERNAL ((selector &rest selector-args) maybe-schema-bit-vector dependency-selector &body body)
  `(let* ((schema-bit-vector
	    (if ,maybe-schema-bit-vector
		(clear-bit-vector ,maybe-schema-bit-vector)
		(make-schema-bit-vector)))
	  (selected-items
	    (item-numbers-to-items
	      (funcall ,selector ,@selector-args))))
     ;; Set all the schemas selected.
     (loop for item in selected-items
	   for s = (funcall ,dependency-selector item)
	   do (bit-ior schema-bit-vector s t))	; T -> bash SCHEMA-BIT-VECTOR.
     ,@body))

(defmacro WITH-SOME-RESULT-DEPENDENT-SCHEMAS (((selector &rest selector-args) &optional schema-bit-vector) &body body)
  `(with-some-dependent-schemas-internal (,selector ,@selector-args) ,schema-bit-vector #'item-result-dependent-schemas
     ,@body))

(defmacro WITH-SOME-CONTEXT-DEPENDENT-SCHEMAS (((selector &rest selector-args) &optional schema-bit-vector) &body body)
  `(with-some-dependent-schemas-internal (,selector ,@selector-args) ,schema-bit-vector #'item-context-dependent-schemas
     ,@body))
;;; ---

;;; This isn't used until a few iterators below, but it's here to keep it near WITH-SOME-SCHEMAS.
(defmacro WITH-ALL-BARE-AND-SOME-OTHER-SCHEMAS (selector &body body)
  `(with-some-schemas ,selector
     ;; Set all the bare schemas in our bit vector.
     (loop for i in (all-bare-schema-numbers)	; $OPT:  Precompute this at the start of a run, then not again.
	   do (setf (bit schema-bit-vector i) 1))
     ,@body))

;;; Ditto.
(defmacro MAKE-ALL-BARE-AND-SOME-OTHER-SCHEMAS-YIELDER-AND-RESETTER (selector)
  `(with-all-bare-and-some-other-schemas ,selector
     (make-simple-yielder-and-resetter-from-bit-vector schema-bit-vector)))

;;; This doesn't work so well:  When we start, _all_ schemas are bare, _none_ of them
;;; depend on any items, and so we can't get the ball rolling to spin any off, because we
;;; never look at any of them.
(defun SCHEMAS-DEPENDENT-UPON-CHANGED-ITEMS-GENERATOR ()
  (with-some-schemas #'item-numbers-which-changed-state
    (make-simple-yielder-and-resetter-from-bit-vector schema-bit-vector)))

(defsubst BARE-SCHEMA? (schema)
  (and (schema-context-empty-p schema)
       (schema-result-empty-p schema)))

(defun ALL-BARE-SCHEMA-NUMBERS ()
  ;; I could cheat and use the initial setup logic to tell us this, for any given microworld,
  ;; but I'll compute it here instead.  See also the discussion in the dead-code file about
  ;; how we really only need to check the result for emptiness, instead of calling BARE-SCHEMA?
  (loop for i from 0 below *schema-number*
	for schema = (get-schema i)
	when (bare-schema? schema)
	  collect i))

(defun ALL-BARE-SCHEMAS-PLUS-SCHEMAS-DEPENDENT-UPON-CHANGED-ITEMS-GENERATOR ()
  (make-all-bare-and-some-other-schemas-yielder-and-resetter
    #'item-numbers-which-changed-state))

(defparameter *ALL-SCHEMAS-EVERY-N* 25)

(defun MOD-ALL-BARE-SCHEMAS-PLUS-SCHEMAS-DEPENDENT-UPON-CHANGED-ITEMS-GENERATOR ()
  (if (zerop (mod *clock-tick* *all-schemas-every-n*))
      (all-schema-numbers-generator)
      (all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator)))

;;; Stuff for keeping a history of the items which changed state _last_ iteration,
;;; and the iteration before _that_, etc.  This stuff is independent of the iterator
;;; CHANGED-ITEM-NUMBERS-MOST-SPECIFIC-FIRST-GENERATOR (used in spinoffs)
;;; and so forth, and is used in statistics instead.  It's not really a "meter" per se,
;;; but by abusing the mechanism in this case, it'll get saved & restored, and cleared
;;; when the run starts.  (It's much more properly a piece of the microworld, really,
;;; and maybe I should change it to reflect that.)  The zeroth element of the history
;;; is a bitvector, where a 1 indicates that this item was different on the list iteration
;;; than it is now.  The first element of the history was the zeroth element last iteration,
;;; and so forth.  The zeroth element of the history is thus a bit-vector duplicate of the
;;; list you'd get back from ITEM-NUMBERS-WHICH-CHANGED-STATE.
(def-vector-metering-counter *CHANGED-STATE-ITEM-HISTORY*)
(def-reported-parameter *CHANGED-STATE-ITEM-HISTORY-SIZE* 2)

;;; [&&& I wonder if just FILL might be faster, either in Genera or other architectures?]
(defsubst CLEAR-BIT-VECTOR (vector)
  (bit-xor vector vector vector))

(defun MAKE-EMPTY-CHANGED-STATE-ITEM-HISTORY ()
  ;; What we're simulating here is a resource, of course, without using DEFRESOURCE etc.
  (cond (*changed-state-item-history*
	 (loop for index from 0 below *changed-state-item-history-size*
	       for history = (aref *changed-state-item-history* index)
	       do (clear-bit-vector history)))
	(t
	 (setf *changed-state-item-history*
	       (make-array *changed-state-item-history-size*))
	 (loop for index from 0 below *changed-state-item-history-size*
	       do (setf (aref *changed-state-item-history* index)
			(make-item-bit-vector))))))

(defsubst MAYBE-MAKE-EMPTY-CHANGED-STATE-ITEM-HISTORY ()
  ;; Makes one iff there isn't already one around.  This is safe to call at any time, since
  ;; it will never bash an existing history the way MAKE-EMPTY-CHANGED-STATE-ITEM-HISTORY
  ;; will.  That means this can be called by UPDATED-CHANGED-STATE-ITEM-HISTORY before it
  ;; runs, to make the history the first time we run.  (This is actually every time we start
  ;; a run, not every time we boot, because we made the history a meter that gets automatically
  ;; cleared at the start of a new run.)
  (unless *changed-state-item-history*
    (make-empty-changed-state-item-history)))

(defun MAKE-CHANGED-STATE-ITEM-BIT-VECTOR (input)
  ;; Not the most stunningly efficient way to do this, but this is only a test at the moment...
  (clear-bit-vector input)
  (loop for item-index from 0 below *item-number*
	for item = (get-item item-index)
	while item
	unless (eql (item-current-state item) (item-last-state item))
	  do (setf (bit input item-index) 1)))

;;; This must be called once per iteration.
(defun UPDATE-CHANGED-STATE-ITEM-HISTORY ()
  ;; If the history was long, this would be better implemented as a circular buffer.
  ;; Note that at least we're just copying pointers to the arrays around, instead of
  ;; the arrays themselves, so the overhead of a true circular buffer might predominate
  ;; until the history was at least half a dozen iterations long, which I hope it'll never be.
  ;; We smash TEMP immediately after moving it, but we hang onto it so we don't have
  ;; to cons a new array.  [We're basically doing a ROTATEF, where we don't know until
  ;; runtime how many things we're rotating and hence can't use ROTATEF itself without
  ;; calling the evalutator (yuck).]
  (maybe-make-empty-changed-state-item-history)	; Only does anything if no history whatsoever.
  (let ((temp (aref *changed-state-item-history* (1- *changed-state-item-history-size*))))
    (loop for destination from (1- *changed-state-item-history-size*) downto 1
	  for source = (1- destination)
	  do (setf (aref *changed-state-item-history* destination)
		   (aref *changed-state-item-history* source)))
    (make-changed-state-item-bit-vector temp)
    (setf (aref *changed-state-item-history* 0) temp)))

;;; The idea of passing in the horizon explicitly is to allow us use the same history for two different callers.
;;; One caller might like a history n long, while the other might like some history m  n long,  In this case, we
;;; set *CHANGED-STATE-ITEM-HISTORY-SIZE* to n, and the caller who wants m calls this with its own m.  This
;;; keeps me from having to redundantly keep two histories, or (in the case where m = 1) from having to
;;; special-case the problem and call ITEM-NUMBERS-WHICH-CHANGED-STATE instead.
(defun CHANGED-ITEM-NUMBERS-IN-HISTORY-AS-LIST (&key (horizon *changed-state-item-history-size*)
						(history *changed-state-item-history*))
  (assert (<= horizon *changed-state-item-history-size*)) ; Caller can't ask for more history than we're keeping!
  (enumerate-bit-vector
    (reduce #'bit-ior history :end horizon)))

;;; LIke CHANGED-ITEM-NUMBERS-GENERATOR, but yields all item numbers which have
;;; changed in the whole history (CHANGED-ITEM-NUMBERS-GENERATOR is a special
;;; case that doesn't use the history and yields essentially what only the zeroth
;;; element of the history would yield).  Note that this constant shuffling between
;;; bit-vectors, lists, and arrays is pretty damned inefficient in runtime and consing.
(defun CHANGED-ITEM-NUMBERS-IN-HISTORY-GENERATOR ()
  (make-generator-from-list-producer
    #'changed-item-numbers-in-history-as-list))

;;; Schemas dependent upon changed items in the history (therefore not necessarily
;;; just the prior tick, but maybe a few prior ticks).
     
(defun ALL-BARE-SCHEMAS-PLUS-SCHEMAS-DEPENDENT-UPON-CHANGED-ITEMS-IN-HISTORY-GENERATOR ()
  (make-all-bare-and-some-other-schemas-yielder-and-resetter
    #'changed-item-numbers-in-history-as-list))

;;; Debugging only.  Sees if one generator produces a superset of the other.  Only works
;;; for generators which produce intermediate results as bit-vectors.
;;; 
;;; Errs if subset relationship incorrect.  No useful value returned in non-erring case.
;;; [This was #+Genera'd on 27 Jan 94 to insulate it from HCL, because it calls
;;;  SCL:BIT-VECTOR-SUBSET-P.  I don't want to have to write this unless I have to,
;;;  and, since was only for debugging, I guess it means that I don't have to.]
#+Genera
(defsubst CHECK-BIT-VECTOR-SUBSET (bigger smaller bigger-generator smaller-generator)
  (unless (scl:bit-vector-subset-p smaller bigger)
    (error "~S wasn't a subset of ~S,~&for ~S~&and ~S.~&Strays are ~S.~&"
	   smaller bigger
	   smaller-generator bigger-generator
	   (enumerate-bit-vector
	     (bit-xor (bit-and bigger smaller) bigger))))
  (values))

;;; The bit-vector of schema numbers returned by WITH-ALL-BARE-AND-SOME-OTHER-SCHEMAS,
;;; when applied to the bit-vector of item numbers returned by the outer generator, must be
;;; a subset (perhaps improper) of the likewise operation on the inner generator.  In other words,
;;; the inner iterator must have at least the same bits on that the outer one does, and may have more.
;;; I loosely call them "bigger" and "smaller" below, even though they may be equal, because, if they're
;;; not, "smaller" must be a subst of "bigger".  We return the appropriate yielder etc from the inner.
;;; If the subset relationship doesn't hold, errs so I can investigate.
(defun WEIRD-ABSPSDU-CHANGED-ITEMS-WITH-AND-WITHOUT-HISTORY-GENERATOR ()
  (let ((smaller-generator 'item-numbers-which-changed-state)	; Not "#'", for the sake of the error message.
	(bigger-generator  'changed-item-numbers-in-history-as-list))	; Ditto.
    (with-all-bare-and-some-other-schemas smaller-generator
      (let ((smaller schema-bit-vector))		; Capture smaller bit vector.
	(with-all-bare-and-some-other-schemas bigger-generator
	  (let ((bigger schema-bit-vector))	; Capture bigger bit vector.  (Unnecessary, but allows naming to have parallel construction.)
	    (check-bit-vector-subset		; If this returns, it didn't err, and the subset relationship held.
	      bigger smaller bigger-generator smaller-generator)
	    (make-simple-yielder-and-resetter-from-bit-vector bigger)))))))

;;;; Keeping track of the last time we updated the statistics for any particular schema.

;;; Assuming that this stuff turns out to be useful, we'll want it automatically cleared
;;; at the beginning of a run, and saved between runs, so we'll use meters.  I suspect
;;; that a "production" version of this code would collapse a lot of these meters into
;;; one, though.  For that matter, the whole idea of "meters" assumes that we're going
;;; to want to turn them off eventually and run unmetered (hence faster & using less
;;; space), at which point I'll have to rethink things (maybe splitting DEF-RUN-VARIABLE
;;; into DEF-RUN-VARIABLE-SAVED (in a snapshot) and DEF-RUN-VARIABLE-UNSAVED).
;;; Or maybe I'll just abandon the generality and wire straight into the code, but this
;;; somehow seems cleaner.

;;; For the moment, we'll make this a debugging switch.  At some point, I may force
;;; the code to use this all the time, at which point this can go away.
(def-debugging-switch TRACK-SCHEMA-STAT-UPDATES t)	; For the moment.

(defun MAKE-EMPTY-SCHEMA-TICK-VECTOR (symbol)
  ;; What we're simulating here is a resource, of course, without using DEFRESOURCE etc.
  (cond ((and (boundp symbol)			; Chicken & egg problem during patch loading etc...
	      (symbol-value symbol))
	 (loop for index from 0 below *schema-maximum*
	       do (setf (aref (symbol-value symbol) index) -1)))
	(t
	 (setf (symbol-value symbol)
	       (make-array *schema-maximum* :initial-element -1))))
  (symbol-value symbol))

;;; These are vectors *SCHEMA-MAXIMUM* long, initially all -1.  Each time we update schema
;;; number n, we stick the current clock tick into position n of this vector.  The peculiar
;;; init-function is so we don't simply toss 3 3600-element vectors to the mercy of the GC
;;; every time we start a new run; we'll reuse the arrays, if possible.
(def-scalar-metering-counter *SCHEMA-TICK-VECTOR-POSITIVE*
			     (make-empty-schema-tick-vector '*schema-tick-vector-positive*))
(def-scalar-metering-counter *SCHEMA-TICK-VECTOR-NEGATIVE*
			     (make-empty-schema-tick-vector '*schema-tick-vector-negative*))
(def-scalar-metering-counter *SCHEMA-TICK-VECTOR-CONTEXT*
			     (make-empty-schema-tick-vector '*schema-tick-vector-context*))

;;; These keep track of how many _unique_ schemas were modified on each iteration.
;;; They are _not_ the sum of the various *UEIS-MODIFY-WHEN-xxx* meters, because any
;;; given one of those is incremented _every_ time we modify anything, even if we
;;; update the statistics multiple times on the same schema in a given iteration (e.g.,
;;; if several things in its extended-context or extended-result have changed).
(def-vector-metering-counter *UNIQUE-SCHEMA-STATS-MODIFIED-EACH-ITERATION-POSITIVE*)
(def-vector-metering-counter *UNIQUE-SCHEMA-STATS-MODIFIED-EACH-ITERATION-NEGATIVE*)
(def-vector-metering-counter *UNIQUE-SCHEMA-STATS-MODIFIED-EACH-ITERATION-CONTEXT*)

;;; Analysis function.
(defun AREA-UNDER-TICK-VECTOR (&optional
			       limit
			       (name '*schema-tick-vector-context*)
			       snapshot-spec)
  (let* ((full-meter
	   (named-meter-from-snapshot name snapshot-spec))
	 (full-list
	   (listarray full-meter))
	 (short-list
	   (remove -1 full-list))
	 (real-limit
	   (maximize short-list))
	 (limit
	   (progn
	     (when (and limit
			(> real-limit limit))
	       (format *error-output* "~&Warning:  ~S is ~D, but ~S is ~S.  This will lead to negative areas being summed.~&"
		       'limit limit 'real-limit real-limit))
	     (or limit real-limit)))
	 (differences				; Combining this and the REDUCE by making a loop would be more efficient, but this is easier.
	   (mapcar #'(lambda (item)		; Also, this way we can return just the differences, which might be handy to look at or to plot.
		       (- limit item))
		   short-list))
	 (area
	   (reduce #'+ differences)))
    (values area
	    limit
	    (length differences)
	    differences)))

(def-reported-parameter *SCHEMA-STATS-UPDATED-WITHIN-N-TICKS* 1)

(defun SCHEMAS-WITH-RECENTLY-UPDATED-STATS-GENERATOR ()
  (macrolet ((recent? (i vector)
	       `(let ((elt (aref ,vector ,i)))
		  (and (not (minusp elt))
		       (< (- *clock-tick* elt)
			  *schema-stats-updated-within-n-ticks*)))))
    (let ((schema-numbers nil))
      (loop for i from (1- *schema-number*) downto 0	; 'cause we're using PUSH below.
	    do (when (or (recent? i *schema-tick-vector-positive*)
			 (recent? i *schema-tick-vector-negative*)
			 (recent? i *schema-tick-vector-context*))
		 (push i schema-numbers)))
      (let* ((length (length schema-numbers))
	     (vector (make-array length :initial-contents schema-numbers)))
      (make-simple-yielder-and-resetter vector length)))))

;;; For use by SUMMARIZE-SNAPSHOTS as an :EXTRA-FN.
(defun REPORT-RECENCY-VS-SPINOFF-INNER-LOOPS (snapshot)
  (format nil "Recency/spinoff-inner:  ~D and ~:D"
	  (reported-parameter-from-snapshot snapshot '*schema-stats-updated-within-n-ticks*)
	  (sum-single-vector-meter '*spinoff-inner-loop-counters* snapshot)))

;;; For use by SUMMARIZE-SNAPSHOTS as an :EXTRA-FN.
(defun REPORT-RECENCY-VS-SPINOFF-AND-STATS-INNER-LOOPS (snapshot)
  (format nil "~D, ~:D, and ~:D"
	  (reported-parameter-from-snapshot snapshot '*schema-stats-updated-within-n-ticks*)
	  (sum-single-vector-meter '*spinoff-inner-loop-counters* snapshot)
	  (sum-single-vector-meter '*ueis-inner-loop-counters* snapshot)
	  ))

;;; End of file.
