;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Iterators that are specific to the EYEHAND world.

(in-package :schema)

;;; This is nonetheless in the SCHEMA package, because it calls into many other
;;; things there, and the attention system is also there.  The occasional true
;;; references to items in EYEHAND therefore need package specs.  Too bad.

;;; Note that many of the functions below _must_ be called with the bindings
;;; established by WITH-CURRENT-MICROWORLD in effect!

;;;; Figuring out which item numbers correspond to particular conceptual items.

;;; +++ 

;;; Discovers all the unique item class names.  [&&& This could undoubtedly be made
;;; faster by not consing all the intermediate lists, and (maybe) by sorting and then
;;; removing duplicates "by hand", knowing that the list is sorted, and (perhaps not,
;;; given the behavior of CDR-coded lists) by using destructive instead of
;;; non-destructive list operations---but I don't expect to call this more than once
;;; per run!  (I mean, geez, now that I've finished it, I discover that it _still_ only
;;; takes 50 ms to run!)]
(defun ALL-EYEHAND-ITEM-BASIC-CLASS-NAMES ()
  (let* ((items
	   (current-microworld-primitive-items))
	 (item-names
	   (mapcar #'symbol-name items))
	 (item-names-no-numbers
	   (mapcar #'(lambda (name)
		       (subseq name 0
			       (position-if #'digit-char-p name)))
		   item-names))
	 (item-names-no-duplicates
	   (remove-duplicates item-names-no-numbers :test #'string-equal)))
    (sort item-names-no-duplicates #'string-lessp)))	; Might as well return 'em in alphabetic order...

;;; Caches a class for each unique type of item, also recording each item's number as
;;; a secondary key, using MAYBE-CACHE-ITEM-BASIC-CLASS-AND-NUMBERS-NAMED.
;;;
;;; Note that the table is keyed by symbols in the SCHEMA
;;; package, not EYEHAND!  (I'm not sure yet whether that's a good idea...)
;;;
;;; Note also that this must do substantial work (well, 50 ms) even if the info
;;; is already cached, because it must generate all item class names anyway.
(defun MAYBE-CACHE-ALL-EYEHAND-ITEM-BASIC-CLASSES-AND-NUMBERS ()	; "Toplevel".
  (let ((all-class-names (all-eyehand-item-basic-class-names)))
    (loop for class-name in all-class-names
	  for class-symbol = (intern class-name)	; SCHEMA package, not EYEHAND!
	  do (maybe-cache-item-basic-class-and-numbers-named class-symbol)))
  (values))

;;; Can be the name of a basic class or its set of numbers.
(defun GET-BASIC-ITEM-CLASS-NAMED (key)
  (let ((items (items-of-class key t)))		; Return T if class nonexistent (e.g., not a list).
    (cond ((eq items t)
	   ;; Class definitively didn't exist.
	   (maybe-cache-all-eyehand-item-basic-classes-and-numbers)	; Try to create it...
	   (items-of-class key))		; ...and explode if we still didn't manage to do so.
	  (t
	   items))))

;;; Like WITH-ITEM-NAMES-AND-NUMBERS-OF-CLASS, but fills the cache first if necessary.
(defmacro WITH-BASIC-ITEM-NAMES-AND-NUMBERS-OF-CLASS ((item-names item-numbers key
								  &optional nonexistent-okay)
						      &body body)
  `(progn
     (get-basic-item-class-named ,key)		; Fill the cache, if necessary.
     (with-item-names-and-numbers-of-class (,item-names ,item-numbers ,key ,nonexistent-okay)
       ,@body)))

;;; +++ "Points".  Dotted pairs that describe coordinates.

;;; &&& +++
;;; For the moment, we assume that any given point's coordinate in either axis is
;;; representable as a single digit, for simplicity (and because the item-naming
;;; paradigm elsewhere would also fall apart if this weren't true).
;;;
;;; This means, of course, that as soon as I make a big enough microworld, the
;;; naming paradigm for items (e.g., VF23) is going to break.  I'd have have to either
;;; zero-fill (VF0203), or separate (VF-2-3).
;;;
;;; Another way to attack this is to have DEF-ITEM-2D build a parallel matrix.  Each
;;; element of the matrix contains the item number of the corresponding item.  Then
;;; you can just do the normal aref to find the item number & hence the item (if you
;;; need it).  Hmm.
;;; &&& ---

;;; Transforms the name of some item (which we assume is a string of the sort
;;; "xxxxdd", where there is any length of alphabetic characters followed by _exactly two_
;;; digits, which are the x and y coordinates of the item in some grid) into a point.
(defsubst NAME-TO-POINT (name)			; [ ~ 46 us.  Pretty quick!]
  (symbol-to-name name)
  (let ((digit-position (position-if #'digit-char-p name)))
    (unless digit-position
      (error "Couldn't find any digits in ~S." name))
    (unless (= (- (length name) digit-position) 2)
      (error "Not exactly two digits in ~S." name))
    (let ((x-char (aref name digit-position))
	  (y-char (aref name (1+ digit-position))))
      ;; PARSE-INTEGER would want a string, not a character, and it's kinda slow besides.
      ;; CL specifies that even in non-ASCII repertoires must have the digits in order.
      (let ((x (- (char-code x-char) #.(char-code #\0)))
	    (y (- (char-code y-char) #.(char-code #\0))))
	(cons x y)))))

;;; The inverse of NAME-TO-POINT.  Returns a string, not a symbol.
(defsubst POINT-TO-NAME (point base)
  (let ((x (car point))
	(y (cdr point)))
    (assert (and (<= 0 x 9) (<= 0 y 9)))
    (string-append
      base
      (string (code-char (+ x #.(char-code #\0))))
      (string (code-char (+ y #.(char-code #\0)))))))

;;; Conveniences for the above.
(defsubst NAMES-TO-POINTS (names)
  (mapcar #'name-to-point names))

(defsubst POINTS-TO-NAMES (points base)		; Takes a single base, not a bunch of 'em!
  (mapcar #'point-to-name points (circular-list base)))

;;; +++ The two routines below are complicated by boundary conditions:  we must
;;; prune any points which would lie outside the boundary.  As long as we're
;;; bothering to prune, I've made it somewhat general, but only somewhat:  we can
;;; prune against an upper bound and (optionally) a lower bound (which should
;;; usually be zero), but the bounds are the same in both axes.
;;;
;;; Note that they'll blow out if the point isn't _somewhere_ inside the bounds.
;;; (The OCT routine calls QUAD, and depends upon QUAD to blow out if necessary).

;;; Returns point coordinates in the "quad" around a point (non-diagonal pawn moves).
(defsubst QUAD-AROUND-POINT (point max &optional (min 0))
  (let ((x (car point))
	(y (cdr point))
	(points nil))
    (assert (and (<= min x max) (<= min y max)))
    (loop for x-offset in '(-1 +1)
	  for x-pt = (+ x x-offset)
	  when (<= min x-pt max)
	    do (push (cons x-pt y) points))
    (loop for y-offset in '(-1 +1)
	  for y-pt = (+ y y-offset)
	  when (<= min y-pt max)
	    do (push (cons x y-pt) points))
    points))

;;; Returns point coordinates in the "oct" around a point (diagonal and non-diagonal
;;; pawn moves, e.g., all one-step queen moves).
(defsubst OCT-AROUND-POINT (point max &optional (min 0))
  (let ((x (car point))
	(y (cdr point))
	(points (quad-around-point point max min)))
    (loop for x-offset in '(-1 +1)
	  do (loop for y-offset in '(-1 +1)
		   for x-pt = (+ x x-offset)
		   for y-pt = (+ y y-offset)
		   when (and (<= min x-pt max)
			     (<= min y-pt max))
		     do (push (cons x-pt y-pt) points)))
    points))

;;; +++ The VF items that correspond to the fovea, and nearby points.

(defsubst VF-DIAMETER-AS-LIMIT ()
  (declare (special eyehand::*vf-diameter*))
  (1- eyehand::*vf-diameter*))

;;; Returns those item numbers of the VF items corresponding to the location of the fovea.
;;;
;;; [&&& Uh, strictly speaking, we _know_ that the MAX arg in this particular call to
;;; QUAD-AROUND-POINT is unnecessary, since it's defined that the fovea fit in the
;;; coarse visual field.  But since we've gotta supply it anyway, this'll tend to be a
;;; last-ditch check that the *VF-DIAMETER* is indeed big enough...]
(defun VF-COARSE-FOVEAL-ITEM-NAMES ()		; [ ~ 4ms ]
  (let* ((max (vf-diameter-as-limit))
	 (center (/ max 2))
	 (center-point (cons center center))
	 (points				; X, Y coordinates
	   (append (list center-point)
		   (quad-around-point center-point max)))
	 (strings
	   (mapcar #'(lambda (point)
		       (format nil "VF~D~D"
			       (car point)
			       (cdr point)))
		   points))
	 (symbols
	   (item-strings-to-item-symbols strings)))
    symbols))

;;; Caches the locale computed by VF-COARSE-FOVEAL-ITEM-NAMES.
;;; Returns the list of numbers in the locale.
(defun MAYBE-CACHE-VF-COARSE-FOVEAL-ITEM-NAMES-AND-NUMBERS (&key (key 'vf-coarse-fovea))
  (let ((names (vf-coarse-foveal-item-names)))
    (maybe-cache-arbitrary key names)
    (maybe-cache-numbers-for-class-named key)))

;; Returns the quad "halos" around all the points, pruning duplicates.
;; [&&& We could do maybe do better on the duplicate pruning with sorting or
;; something, but this is no doubt good enough for now.]
(defun POINTS-AND-THEIR-QUADS (points max &optional (min 0))
  (let* ((halos
	   (mapcan #'quad-around-point points
		   (circular-list max)
		   (circular-list min)))
	 (everybody
	   (append points halos)))
    (remove-duplicates everybody :test #'equal)))

;;; Returns the entire set of item numbers associated with the VF coarse foveal
;;; region and those quads around it.
;;;
;;; &&& Note that all of this "coordinate conversion" between items, names, and
;;; numbers in both directions is hardly efficient.  Doesn't much matter for this
;;; particular case, because it gets cached, but in general, this seems silly (see
;;; above discussion about making a secondary matrix or two to hold this information
;;; directly, when the items are created).  Granted, this only takes about 35 ms,
;;; starting from an empty cache.
(defun MAYBE-CACHE-VF-COARSE-FOVEA-AND-HALO (&key (fovea-key 'vf-coarse-fovea)	; "Toplevel".
					     (halo-key 'vf-coarse-fovea-and-quads))
  (maybe-cache-vf-coarse-foveal-item-names-and-numbers :key fovea-key)	; Compute where the fovea is.
  (or (items-of-numbered-class-or-nil halo-key)
      (let* ((foveal-items			; Get names of the foveal items.
	       (items-of-class fovea-key))
	     (foveal-points			; Get coordinates from names.
	       (names-to-points foveal-items))
	     (foveal-and-halo-points		; Get above coordinates unioned with their quads.
	       (points-and-their-quads foveal-points (vf-diameter-as-limit)))
	     (foveal-and-halo-item-strings	; Now get everybody's name again.
	       (points-to-names foveal-and-halo-points "VF"))
	     (foveal-and-halo-item-symbols	; *sigh*
	       (item-strings-to-item-symbols foveal-and-halo-item-strings)))
	(maybe-cache-arbitrary halo-key foveal-and-halo-item-symbols) ; Now cache all the names...
	(maybe-cache-numbers-for-class-named halo-key)))) ; ...and then cache item numbers for the names.

;;; This is all VF items that _aren't_ in the fovea or its halo.
;;;
;;; Computes item names as well as item numbers, for completeness, even though I don't think we need 'em.
(defun MAYBE-CACHE-ALL-VF-BUT-NON-FOVEAL-HALO (&key (non-foveal-halo-key 'vf-non-foveal-halo))	; [ ~ 80 ms until cached, then ~ 12 ms]
  (maybe-cache-vf-coarse-fovea-and-halo)
  (or (items-of-numbered-class-or-nil non-foveal-halo-key)
      (let* ((vf-item-names
	       (get-basic-item-class-named 'vf))
	     (foveal-halo-item-names
	       (items-of-class 'vf-coarse-fovea-and-quads))	; Not GET-BASIC-ITEM-CLASS-NAMED because this is a locale, not a basic class.
	     (non-foveal-halo-item-symbols
	       (set-difference vf-item-names foveal-halo-item-names)))	; The names are symbols, not strings, so EQL will work fine.
	(maybe-cache-arbitrary non-foveal-halo-key non-foveal-halo-item-symbols)	; Now cache all the names...
	(maybe-cache-numbers-for-class-named non-foveal-halo-key))))	; ...and then cache item numbers for the names.

;;; Do the INTERNs for these guys at compile-time, since it's slow.
(defsubst ALL-VF-ITEM-NUMBERS ()
  (get-basic-item-class-named
    '#.(class-name-to-numbered-class-name 'vf)))

(defsubst ALL-VF-COARSE-FOVEAL-ITEM-NUMBERS ()	; E.g., only the five coarse items, not all the FOVn detail items.
  (get-basic-item-class-named
    '#.(class-name-to-numbered-class-name 'vf-coarse-fovea)))

(defsubst ALL-VF-COARSE-FOVEAL-HALO-ITEM-NUMBERS ()
  (items-of-class				; This is a locale, not a basic class.
    '#.(class-name-to-numbered-class-name 'vf-coarse-fovea-and-quads)))

;;; This doesn't need "coarse" in its name because it's excluding the whole fovea
;;; anyway, so it doesn't matter.
(defsubst ALL-VF-NON-FOVEAL-HALO-ITEM-NUMBERS ()
  (items-of-class				; This is a locale, not a basic class.
    '#.(class-name-to-numbered-class-name 'vf-non-foveal-halo)))

;;; +++ Initializing most of the caches at once.  (I'm not quite sure I really need to do this.
;;; +++ After all, each codelet that needs something from the cache can just ask for it, and
;;; +++ it'll be computed on demand...  That's part of the point...  But it'll only work if I take
;;; +++ care to write everything with GET- forms that check for & compute if necessary
;;; +++ or something, and that seems a lot of work.  [Maybe I can automate it?])

(def-post-microworld-init EYEHAND::FILL-EYEHAND-ITEM-CLASS-CACHE ()	; Must be in EYEHAND:.
  (maybe-cache-all-eyehand-item-basic-classes-and-numbers)
  (maybe-cache-vf-coarse-fovea-and-halo)
  (maybe-cache-all-vf-but-non-foveal-halo)
  (values))

;;; +++ Objects in the visual field, and nearby points.

;;; This takes advantage of the fact that the coarse visual field is mostly empty, and
;;; that any item that's on in it reflects some object, which is by definition
;;; interesting.  It is therefore embedding domain knowledge in the mechanism!  This
;;; would, of course, give GLD the heebie jeebies.
;;;
;;; It doesn't matter if objects are only one pixel wide in the coarse visual field, but
;;; of course they all are anyway.
;;;
;;; &&& This is done in a rather inefficient way (so what else is new?).  Rather than
;;; asking the guts of the microworld to tell us exactly where all of the objects are,
;;; we just iterate over the VF, looking for items which are on, and then
;;; backtranslating to their names etc (including looking up their spatial position
;;; from the name, etc etc etc).  Note that, of course, we never ever cache this
;;; information, since it'll likely change on every clock tick.

;;; &&& It's tempting to just look up the names, not the numbers, here, since our
;;; caller below only needs the names.  But _we_ need the numbers to determine
;;; whether the item is on or not, so we might as well make 'em available...
(defun VF-ITEMS-ON ()
  (with-basic-item-names-and-numbers-of-class (names numbers 'vf)
    (loop for name in names
	  for number in numbers
	  when (state-on-p (get-item-current-state number))
	    collect name into names-on and
	  collect number into numbers-on
	  finally
	    (return (values names-on numbers-on)))))

(defun VF-ITEMS-ON-WITH-HALOS-NUMBERS ()	; [ Only ~ 16 ms, despite the horribly inefficient lookups... ]
  (multiple-value-bind (names-on numbers-on)
      (vf-items-on)
    numbers-on					; Huh.  Guess I didn't need it after all.  *sigh*
    (let* ((item-points
	     (names-to-points names-on))
	   (item-and-halo-points
	     (points-and-their-quads item-points (vf-diameter-as-limit)))
	   (item-and-halo-names
	     (points-to-names item-and-halo-points "VF"))
	   (item-and-halo-numbers
	     (numbers-of-items-named item-and-halo-names)))
      item-and-halo-numbers)))

;;; +++ Objects whose state has changed "recently".

(def-reported-parameter *VF-ONLY-CHANGED-STATE-ITEM-HISTORY-SIZE* 2
  (assert (<= *vf-only-changed-state-item-history-size* *changed-state-item-history-size*)))

;;; &&& Can't combine this with a later call to CHANGED-ITEM-NUMBERS-IN-HISTORY-AS-LIST
;;; in the name of optimization, since here we may be calling it with a different
;;; horizon than the rest of the items.
(defun CHANGED-VF-ITEM-NUMBERS-AS-LIST ()
  (intersection (all-vf-item-numbers)
		(changed-item-numbers-in-history-as-list
		  :horizon *vf-only-changed-state-item-history-size*)))

(defun CHANGED-HALOED-VF-ITEM-NUMBERS-AS-LIST ()
  (let* ((changed-numbers
	   (changed-vf-item-numbers-as-list))
	 (changed-names
	   (item-numbers-to-names changed-numbers))
	 (changed-points
	   (names-to-points changed-names))
	 (changed-and-halo-points
	   (points-and-their-quads changed-points (vf-diameter-as-limit)))
	 (changed-and-halo-names
	   (points-to-names changed-and-halo-points "VF"))
	 (changed-and-halo-numbers
	   (numbers-of-items-named changed-and-halo-names)))
    changed-and-halo-numbers))

;;; +++ All "interesting items".

;;; Since this is going to be fodder for an iterator, we don't need their names.
;;; &&& Might as well be a defun, given that UNION is gonna be slow anyway (and
;;; considering that UNION is itself a subst, in Genera).  Note also that doing it with
;;; UNION is much faster (at least in Genera) than consing up the list and using
;;; REMOVE-DUPLICATES on it; they're probably both O(n^2), but the UNION case
;;; gets much shorter input lists...
(defun VF-INTERESTING-ITEM-NUMBERS ()		; [ ~ 30 ms all told.]
  (union (maybe-cache-vf-coarse-foveal-item-names-and-numbers)
	 (union (vf-items-on-with-halos-numbers)
		(changed-vf-item-numbers-as-list))))

;;; For the moment, this is defined as all _changed_ item numbers (where
;;; _changed_ uses the default horizon for all items, not for VF items), minus all VF
;;; item numbers, plus all "interesting" VF item numbers.
(defun ALL-INTERESTING-ITEM-NUMBERS ()		; [ ~ 30 ms.]
  (union (set-difference (changed-item-numbers-in-history-as-list)
			 (all-vf-item-numbers))
	 (vf-interesting-item-numbers)))

;;; +++ Finally, the iterator that makes use of the above machinery.

;;; *sigh*  This actually causes _more_ work, of course, since it's selecting _more_
;;; items than just changed-items did, but I didn't fully appreciate this until it was written...
(defun ALL-INTERESTING-ITEM-NUMBERS-GENERATOR ()
  (make-generator-from-list-producer
    #'all-interesting-item-numbers))

(defmacro WITH-UEIS-INTERESTING-ITEMS-ALL-SCHEMAS-GENERATORS (&body body)
  `(with-ueis-schema-number-generator 'all-schema-numbers-generator
     (with-ueis-item-number-generator 'all-interesting-item-numbers-generator
       ,@body)))

;;; +++ More iterators.

;;; An actually _optimized_ version of this would not recompute this list each time,
;;; since it's exactly the same, unless we've added any schemas.  An optimized
;;; version would compute whether or not the schema qualified, once, and stick it on
;;; the actual schema (or incrementally add to a list of such schemas, more likely).
;;; However, for testing purposes, we'll compute this every iteration (we can always
;;; use the logic that computes it to later compute the cache).
;;;
;;; Note that there are two ways of computing this information:  By asking each
;;; schema which items it depends upon, and by asking each item which schemas
;;; depend on it.  This one asks each schema, not each item.  [&&& What about
;;; changed items?  Think about this...] To see how to do this in a much more
;;; optimized fashion, look at VF-CHANGED-HALOED-SCHEMAS-AS-BIT-VECTOR, rather
;;; than by using these macros.

(defmacro SCHEMA-RESULT-ONLY-DEPENDS-ON-ITEM-NUMBERS? (schema item-number-generator &rest item-number-generator-args)
  ;; A more straightforward implementation might just make a list of items and
  ;; check for a match, but this version is written not to cons.  This is a macro
  ;; so we can optimize out the call to the item-number-generator if we don't
  ;; need it (e.g., if the schema has an empty result anyway).
  `(block schema-result-depends-on-item-numbers?
     (unless (schema-result-empty-p ,schema)
       (let ((item-numbers (funcall ,item-number-generator ,@item-number-generator-args)))
	 (cond ((schema-result-conj-p ,schema)	; Hard case:  this is a conj.
		(let* ((conj-index (schema-result-item ,schema))
		       (conj (get-conj conj-index))
		       (conj-item-state-array (conj-item-array conj))
		       (max (ceiling *item-number* *states-in-fixnum*)))
		  ;; Walk down the item state array, quickly, returning NIL if we find something in the conj that
		  ;; depends on something that isn't in VF.  Stolen from STATE-ARRAY-COPY-SOME-FLAG.
		  ;; Slightly faster than that one, because this only walks up to the number of fixna representing
		  ;; the number of existing items, not all items.  [&&& Hmm.  Should I maybe optimize that one in
		  ;; a similar fashion?]  The slight overshoot on the last fixnum is immaterial, because all items
		  ;; there are in state "unknown".
		  (let ((item-number 0))
		    (dotimes (x max)
		      (let ((num (aref conj-item-state-array x)))
			(dotimes (y *states-in-fixnum*)
			  (unless (state-unknown-p (state-record-get-state num y))
			    (unless (member item-number item-numbers)
			      (return-from schema-result-depends-on-item-numbers? nil)))
			  (incf item-number)))))
		  t))				; Must all depend on something in VF.
	       (t				; Easy case:  this is a single item.
		(member (schema-result-item ,schema) item-numbers)))))))

;;; This is very like SCHEMA-RESULT-ONLY-DEPENDS-ON-ITEM-NUMBERS?, but returns true
;;; if _any_ of the item numbers in this schema's result match, rather than if they _all_ do.
(defmacro SCHEMA-RESULT-MAY-DEPEND-ON-ITEM-NUMBERS? (schema item-number-generator &rest item-number-generator-args)
  ;; A more straightforward implementation might just make a list of items and
  ;; check for a match, but this version is written not to cons.  This is a macro
  ;; so we can optimize out the call to the item-number-generator if we don't
  ;; need it (e.g., if the schema has an empty result anyway).
  `(block schema-result-depends-on-item-numbers?
     (unless (schema-result-empty-p ,schema)
       (let ((item-numbers (funcall ,item-number-generator ,@item-number-generator-args)))
	 (cond ((schema-result-conj-p ,schema)	; Hard case:  this is a conj.
		(let* ((conj-index (schema-result-item ,schema))
		       (conj (get-conj conj-index))
		       (conj-item-state-array (conj-item-array conj))
		       (max (ceiling *item-number* *states-in-fixnum*)))
		  ;; Walk down the item state array, quickly, returning NIL if we find something in the conj that
		  ;; depends on something that isn't in VF.  Stolen from STATE-ARRAY-COPY-SOME-FLAG.
		  ;; Slightly faster than that one, because this only walks up to the number of fixna representing
		  ;; the number of existing items, not all items.  [&&& Hmm.  Should I maybe optimize that one in
		  ;; a similar fashion?]  The slight overshoot on the last fixnum is immaterial, because all items
		  ;; there are in state "unknown".
		  (let ((item-number 0))
		    (dotimes (x max)
		      (let ((num (aref conj-item-state-array x)))
			(dotimes (y *states-in-fixnum*)
			  (unless (state-unknown-p (state-record-get-state num y))
			    (when (member item-number item-numbers)
			      (return-from schema-result-depends-on-item-numbers? t)))	; Result dependent on at least one of the item numbers.
			  (incf item-number)))))
		  nil))				; Result didn't depend on any of the item numbers.
	       (t				; Easy case:  this is a single item.
		(member (schema-result-item ,schema) item-numbers)))))))

; ;;; A "VF-focused-schema" is a schema whose _only_ result items are in the VF.
; ;;; Any other schema (those which predict no VF items, or which have additional
; ;;; non-VF items in their predictions) do _not_ qualify.
; (defsubst VF-FOCUSED-SCHEMA? (schema)
;    (schema-result-only-depends-on-item-numbers? schema #'all-vf-item-numbers))

;;; A "VF-non-foveal-halo-schema" is a like a VF-focused-schema, but its _only_
;;; result items must be in the VF yet _not_ in the fovea or its halo.  The "non-"
;;; therefore applies to the AND of "foveal" and "foveal halo" and hence means
;;; "non-(fovea-and-its-halo)".
;;;
;;; Note that VF-CHANGED-HALOED-SCHEMAS-AS-BIT-VECTOR shows how to do this in
;;; a much more optimized fashion.
(defsubst VF-NON-FOVEAL-HALO-SCHEMA? (schema)
  (schema-result-only-depends-on-item-numbers? schema #'all-vf-non-foveal-halo-item-numbers))

(defun ALL-VF-NON-FOVEAL-HALO-SCHEMAS ()		; Debugging.  [ ~600 ms!]
  (loop for index from 0 below *schema-number*
	for schema = (get-schema index)
	when (vf-non-foveal-halo-schema? schema)
	  collect index))

(defun NAME-ALL-VF-NON-FOVEAL-HALO-SCHEMAS ()		; Debugging.
  (mapcar #'(lambda (index)
	      (schema-print-name (get-schema index)))
	  (all-vf-non-foveal-halo-schemas)))

(defun SHOW-ALL-VF-NON-FOVEAL-HALO-SCHEMA-NAMES ()	; Debugging.
  (let* ((names (name-all-vf-non-foveal-halo-schemas))
	 (length (length names))
	 (vector (make-array length :initial-contents names)))
    (show-things-down 0 length vector #'identity)))

;;; A "VF-changed-haloed-schema" is a schema which has as its result in which
;;; _at least one result item_ is a changed item or its halo.  (This is an OR-type
;;; of thing, unlike VF-NON-FOVEAL-HALO-SCHEMA?, which is an AND-type thing.)
;;;
;;; Note that, because the changed, haloed item numbers is _not_ cached (it changes
;;; every iteration), if this is being called in a loop, the caller should compute it once
;;; and supply it.
;;;
;;; Um, wouldn't a faster way to do this have to do with the business about items
;;; knowing which schemas depend upon them?  ---YES:  The two functions below
;;; have been OBSOLETED by VF-CHANGED-HALOED-SCHEMAS-AS-BIT-VECTOR,
;;; ***AND*** that function below was what finally made me discover that the whole
;;; dependency mechanism was just totally broken from the start (never noticed
;;; result dependencies in context spinoffs, and other such problems).
(defsubst VF-CHANGED-HALOED-SCHEMA? (schema &optional (changed (changed-haloed-vf-item-numbers-as-list)))
  (schema-result-may-depend-on-item-numbers? schema #'identity changed))

(defun ALL-CHANGED-HALOED-SCHEMAS ()		; Debugging.  [ ~240 ms]
  (let ((changed (changed-haloed-vf-item-numbers-as-list)))
    (loop for index from 0 below *schema-number*
	  for schema = (get-schema index)
	  when (vf-changed-haloed-schema? schema changed)
	    collect index)))

;;; An attempt to speed things up over the two functions above.
(defun VF-CHANGED-HALOED-SCHEMAS-AS-BIT-VECTOR ()    ; [ ~ 26 ms]
  (with-some-result-dependent-schemas ((#'changed-haloed-vf-item-numbers-as-list))
    schema-bit-vector))

;;; We'll take the compromise stance here between recomputing unchanging
;;; information every iteration (ALL-VF-NON-FOVEAL-HALO-SCHEMAS takes about 600 ms to
;;; run, with 3600 schemas) and building it into the schema data structure (and only
;;; updating it when we spin off a result schema, which is the only time the
;;; information can change).  Instead, we keep a local cache, as a meter.
;;; Note that this could just as well be a bitvector, but there's only one of them,
;;; so space isn't an issue, and a normal array is faster to access.
(def-scalar-metering-counter *NON-FOVEAL-HALO-VF-SCHEMAS*
			     (make-empty-schema-tick-vector '*non-foveal-halo-vf-schemas*))
(def-scalar-metering-counter *NON-FOVEAL-HALO-VF-SCHEMAS-LIMIT* 0)	; One above the highest schema index checked so far.

(defun UPDATE-NON-FOVEAL-HALO-VF-SCHEMAS-CACHE ()
  (maybe-cache-all-vf-but-non-foveal-halo)
  (when (> *schema-number* *non-foveal-halo-vf-schemas-limit*)
    (loop for index from *non-foveal-halo-vf-schemas-limit* below *schema-number*
	  when (vf-non-foveal-halo-schema? (get-schema index))
	    do (setf (aref *non-foveal-halo-vf-schemas* index) 1))
    (setf *non-foveal-halo-vf-schemas-limit* *schema-number*)))

;;; Assumes that the cache is up to date already.
(defsubst NON-FOVEAL-HALO-VF-SCHEMA-INDEX? (index)	; &&& I suppose, were this folded into something else, that I could make it ...
  (= (aref *non-foveal-halo-vf-schemas* index) 1))	; ... an array-reg under Genera.  But the speed advantage is minimal.

;;; Conses up a list, like the rest of the iterators.  So sue me.
(defun NON-VF-PLUS-INTERESTING-VF-SCHEMA-NUMBERS ()
  (update-non-foveal-halo-vf-schemas-cache)
  (let ((vf-changed-haloed-schemas-bit-vector
	  (vf-changed-haloed-schemas-as-bit-vector)))
    (loop for index from 0 below *schema-number*
	  for vf? = (non-foveal-halo-vf-schema-index? index)
	  when (or (not vf?)			; Not VF, so it's gotta be useful.
		   (and vf?
			(not (zerop (bit vf-changed-haloed-schemas-bit-vector index)))))
	    collect index)))

(defun NON-VF-PLUS-INTERESTING-VF-SCHEMAS-GENERATOR ()
  (make-generator-from-list-producer
    #'non-vf-plus-interesting-vf-schema-numbers))

;;; For any given set c of changed items, we update stats in:
;;; . all bare (no-result) schemas
;;; . all result schemas whose result mentions something other than vf
;;;   (this includes conj's with at least one non-vf item)
;;; . all result schemas whose result mentions vf, iff the particular
;;;   vf mentioned is in the set hfvf of haloed-foveal points in the vf, or
;;;   the set hcvf of haloed, changed items in the vf.

;;;; More iterators & stuff, this time for goals.

(defun ALL-ACTIVE-VF-COARSE-FOVEAL-ITEM-NUMBERS ()
  (loop for item-index in (all-vf-coarse-foveal-item-numbers)
	for item = (get-item item-index)
	while item
	when (state-on-p (item-current-state item))
	  collect item-index))

;;; A more efficient way of just asking if any of 'em are on.
(defun ANY-ACTIVE-VF-COARSE-FOVEAL-ITEMS? ()
  (loop for item-index in (all-vf-coarse-foveal-item-numbers)
	for item = (get-item item-index)
	while item
	thereis (state-on-p (item-current-state item))))  

;;; +++ Stuff for quickly building up subsets of item numbers to use.
;;; +++ If you add anything here, note the DEF-POST-MICROWORLD-INIT at the end.

;;; Returns the list of item numbers for the new class.
;;; This is handy for things that "should" be basic classes, but aren't, because
;;; their names are related by last letter, rather than by numbers (which is how
;;; ALL-EYEHAND-ITEM-BASIC-CLASS-NAMES must make its decision).
(defsubst UNION-CLASSES (union-name &rest class-names)
  (or (items-of-numbered-class-or-nil union-name)
      (let ((all-item-names
	      (loop for class-name in class-names
		    append (get-basic-item-class-named class-name))))
	(maybe-cache-arbitrary union-name all-item-names)
	(maybe-cache-numbers-for-class-named union-name))))

;;; More eye stuff.
(defsubst ALL-VF-AND-VP-ITEM-NUMBERS ()
  (union-classes 'vf-and-vp 'vf 'vp))

(defsubst ALL-FINE-FOVEA-ITEM-NUMBERS ()
  (union-classes 'fine-fovea 'fovb 'fovf 'fovl 'fovr 'fovx))

(defsubst ALL-EYE-ITEM-NUMBERS ()
  (union-classes 'all-eye 'vf-and-vp 'fine-fovea))

;;; Hand stuff.
(defsubst ALL-TACT-ITEM-NUMBERS ()
  (union-classes 'tact 'tactb 'tactf 'tactl 'tactr))

(defsubst ALL-GRASP-ITEM-NUMBERS ()
  (union-classes 'grasp 'hcl 'hgr))

(defsubst ALL-TEXT-ITEM-NUMBERS ()		; This is already a basic class.
  (get-basic-item-class-named
    '#.(class-name-to-numbered-class-name 'text)))

(defsubst ALL-HAND-ITEM-NUMBERS ()
  (union-classes 'all-hand 'hp 'tact 'text 'grasp))

;;; If I was clever, I'd make each of the that we call in here just a
;;; DEF-POST-MICROWORLD-INIT directly, since they'll be called in order, although of
;;; course then I'd have to put 'em in the EYEHAND package or make a macro that did
;;; both (*sigh*).
(def-post-microworld-init EYEHAND::FILL-MORE-EYEHAND-ITEM-CLASS-CACHE ()	; Must be in EYEHAND:.
  (all-vf-and-vp-item-numbers)
  (all-fine-fovea-item-numbers)
  (all-eye-item-numbers)
  (all-tact-item-numbers)
  (all-grasp-item-numbers)
  (all-hand-item-numbers)
  (values))

;;; End of file.
