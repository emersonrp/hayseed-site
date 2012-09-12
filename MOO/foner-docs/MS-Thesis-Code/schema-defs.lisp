;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

(in-package :schema)

;;;; Debugging output control.

(defvar-exported *OUTPUT-STREAM* t)		; This is handy for microworlds to use.

;;; The macros generated exist so that it's guaranteed that I either set
;;; or reset the variable when I think I've done so (e.g., if I accidentally
;;; do a SETF in the wrong package in a Listener, I get no warning, but
;;; I can't accidentally run a function that just isn't defined there...)

#+Genera
(defmacro PUSHNEW-CAR-REPLACE (item vector &rest adjoin-keywords)
  `(pushnew ,item ,vector :key #'car :replace t ,@adjoin-keywords))

#-Genera
(defmacro PUSHNEW-CAR-REPLACE (item vector &rest adjoin-keywords)
  `(let ((where (assoc (car ,item) ,vector ,@adjoin-keywords)))
    (cond (where
	   (rplacd where (cdr ,item))
	   ,vector)
	  (t
	   (push ,item ,vector)))))

(defvar *SWITCHES* nil)

;;; Note that the default is evaluated at compile time!
(defmacro-definer DEFSWITCH (switch-name switch-default macro-suffix)
  (flet ((build (prefix)
	   (intern-format "~A~A"
			  prefix
			  (symbol-name macro-suffix))))
    `(progn
       (pushnew-car-replace (list ',switch-name ,switch-default ',macro-suffix) *switches*)
       (defparameter ,switch-name ,switch-default)
       (defmacro ,(build "without-") (&body body)
	 `(let ((,',switch-name nil))
	    ,@body))
       (defmacro ,(build "with-") (&body body)
	 `(let ((,',switch-name t))
	    ,@body)))))

(defvar *DEBUGGING-SWITCHES* nil)

;;; Note that the default is evaluated at compile time!
(defmacro-exported-definer DEF-DEBUGGING-SWITCH (name default)
  (let* ((variable-name-nostars (intern-format "~A-enabled" name))
	 (format-macro-name     (intern-format "~A-format"  name))
	 (if-macro-name         (intern-format "~A-if"      name))
	 (variable-name         (intern-format "*~A*" variable-name-nostars)))
    `(progn
       (pushnew-car-replace (list ',variable-name ,default) *debugging-switches*)
       (defswitch ,variable-name ,default ,variable-name-nostars)
       (defmacro ,format-macro-name (&rest args)
	 args					; Avoid compiler warnings.
	 `(when ,',variable-name
	    (safe-format *output-stream* ,@args)))
       (defmacro ,if-macro-name (&rest args)
	 args					; Avoid compiler warnings.
	 `(when ,',variable-name
	    (if ,@args))))))

(def-debugging-switch SCHEMA            t)
(def-debugging-switch FLAG-ARRAY        nil)
(def-debugging-switch STATE-ARRAY       nil)
(def-debugging-switch ITEM              nil)
(def-debugging-switch CONJ              nil)
(def-debugging-switch SYN-ITEM          nil)
(def-debugging-switch APPLICABLE        nil)
(def-debugging-switch ACCESSIBILITY     nil)
(def-debugging-switch ACTIVATED         nil)
(def-debugging-switch RESULT            nil)
(def-debugging-switch RELIABILITY       nil)
(def-debugging-switch PREDICTED-RESULTS nil)
(def-debugging-switch EXT-STATS         nil)
(def-debugging-switch EXT-CONJ-STATS    nil)
(def-debugging-switch SHOW-ITEMS        nil)
(def-debugging-switch DEBUG             nil)

(defun SUMMARIZE-DEBUGGING-SWITCHES ()
  (let* ((longest
	   (longest-printed-rep *debugging-switches*
				#'(lambda (entry)
				    (symbol-name (car entry)))))
	 (col-a (+ longest 1))
	 (col-b (+ longest 10)))
    (safe-format t
      (italic-format nil "~&Name~VTCurrent~VTDefault~&"
		     col-a col-b))
    (loop for (switch-symbol default) in (reverse *debugging-switches*)
	  do (safe-format t "~&~A~VT~A~VT~A~&"
			  (symbol-name switch-symbol)
			  col-a
			  (symbol-value switch-symbol)
			  col-b
			  default)))
  (values))

(defun RESET-DEBUGGING-SWITCHES-TO-DEFAULTS ()
  (loop for (switch-symbol default) in *debugging-switches*
	do (set switch-symbol default))
  (values))

;;; +++ Random additions to the debugging macros above.

;;; These two aren't conditional.
(defmacro MAIN-FORMAT (&rest args)
  `(safe-format *output-stream* ,@args))

(defmacro MAIN-FINISH ()
  `(finish-output *output-stream*))

;;; These three have a particular consequent hardwired in.
(defmacro PRINT-FLAG-ARRAY (&rest args)
  args						; Ignored.
  `(when *flag-array-enabled*
     (print-flag-array-enabled ,@args)))

(defmacro PRINT-STATE-ARRAY (&rest args)
  args						; Ignored.
  `(when *state-array-enabled*
     (print-state-array-enabled ,@args)))

(defmacro PRINT-PREDICTED-RESULTS (&rest args)
  args						; Ignored.
  `(when *predicted-results-enabled*
     (print-predicted-results-enabled ,@args)))

;;; ---

;;;; Microworld-independent inits that must be executed when starting a new run.

(defvar *MICROWORLD-INDEPENDENT-INIT-FUNCTIONS* nil)

;;; Defines a function that will be run when starting a new run, regardless of the
;;; microworld.  It will run _before_ the microworld itself is initialized.
;;; 
;;; The list is run in REVERSE ORDER, so that this macro can PUSH things on it
;;; in the order in which they are defined, yet have everything actually RUN
;;; in that order, too.
(defmacro-exported-definer DEF-MICROWORLD-INDEPENDENT-INIT (fn-name (&rest arglist) &body body)
  `(progn
     (defun ,fn-name (,@arglist)
       ,@body)
     (pushnew ',fn-name *microworld-independent-init-functions*)))

;;;; DIfferent metering support.  This stuff does iteration metering,
;;;; not runtime metering, and saves its results in snapshots.

;;; Note that the general strategy here is similar to the way in which schema state
;;; variables are snapshotted, with the difference that we assume here that all
;;; sequences can (and must) simply be copied with copy-seq (e.g., that there's no
;;; internal bashable structure to the elements of the sequence, which we assume
;;; are just numbers or somesuch).  If this is wrong, then I'll have to change this to
;;; use a similar pick-yer-copier mechanism.  We don't just lump all this stuff into
;;; the schema state variables, because I want support for ignoring unrecognized
;;; metering variables etc, since they're likely to change often and I don't want to
;;; spuriously invalidate prior dumps because of this.  (I suppose I _could_ just hack
;;; the schema state variable code in a similar fashion, but this seems clearer and
;;; keeps everything related to metering together...)

;;; Also note that, because the metering stuff here provides a way of putting
;;; arbitrary data that gets special handing into a snapshot, it's become a catchall for
;;; many things that aren't, strictly speaking, meters at all.  Current examples
;;; include "parametric-info" (which record current settings of variables that
;;; change how certain algorithms work, are reported in logs, are saved but _not_
;;; restored in snapshots, and are actually collections of _other_ sets of variables
;;; [such as *REPORTED-PARAMETERS* and *ITERATOR-GENERATORS-IN-USE*] rather
;;; than individual settings) and "limits" (which record current limits to various
;;; array sizes, are _not_ reported in logs, are saved _and_ restored from
;;; snapshots, and have default setting for the case of reading snapshots that were
;;; saved before this was added).

;;; Presumably, if I ever punt the metering code, the above "catchall" applications will
;;; probably still be necessary, so punting the metering code should probably involve a
;;; debugging variable that simply turns off the loading or dumping of only the actual
;;; meters, rather than not calling any of the code at all.

;;; +++ Variables that get checked to assure that we don't define the same name for
;;; +++ more than one of them (because they're all saved in a bunch, and we wouldn't
;;; +++ know how to deal with them if they were allowed to have name collisions).

(defvar *ANCILLARY-COLLISION-CHECKED-VARIABLES* nil)

(defmacro-definer DEF-COLLISION-CHECKED-VARIABLE (name)
  `(progn
     (pushnew ',name *ancillary-collision-checked-variables*)
     (defvar ,name nil)))

;;; +++ True meters:
(def-collision-checked-variable *SCALAR-METERING-COUNTERS*)		; Later mods can't bash the values of these.
(def-collision-checked-variable *VECTOR-METERING-COUNTERS*)		; These must be copied for safety.

;;; Things here get reported every REPORT-RELIABLE-SCHEMAS-EVERY-N-ITERATIONS-DEFAULT in the log.
;;; They'd better also be in one of the above!  We push tuples of (SYMBOL, LABEL) into these.  LABEL
;;; should be a FORMAT string whose only arg accepts a number.  Don't newline-terminate; we'll do that elsewhere.
;;; Note that these are _not_ DEF-COLLISION-CHECKED-VARIABLE forms, since they _should_ also be
;;; included in one of the scalar or vector metering counters.
(defvar *SCALAR-METERING-COUNTERS-PERIODICALLY-REPORTED* nil)
(defvar *VECTOR-METERING-COUNTERS-PERIODICALLY-REPORTED* nil)

;;; +++ Limits:
(def-collision-checked-variable *LIMITS*)

;;; +++ Error-checking function:
;;; Warn if this thing already appears somewhere else (where "somewhere else"
;;; means that we defined it once as, say, a scalar metering counter, and are now
;;; attempting to define it also as a vector metering counter---it does _not_ mean
;;; "in some other file":  we don't check for that).  Since they're all snapshotted in a
;;; bunch, we can't have duplicate ideas of what things are supposed to be.
(defun ERROR-IF-ALREADY-DEFINED-ELSEWHERE (name variable-we-are-defining-for)
  (loop for variable in *ancillary-collision-checked-variables* 
	when (and (not (eq variable variable-we-are-defining-for))	; &&& Strings vs symbols?
		  (boundp variable)
		  (member name (symbol-value variable) :key #'car))	; &&& Strings vs symbols?
	  do (error "~S, being defined for ~S, was previously defined somewhere else,~&in one of ~A~&"
		    name
		    variable-we-are-defining-for
		    (format-print-list nil "~S" *ancillary-collision-checked-variables*)))
  (values))

;;; +++ Now, true meter code.

(defun ALL-METER-NAMES ()			; Returns only the names, not the init-forms.
  (mapcar #'car
	  (append *scalar-metering-counters*
		  *vector-metering-counters*)))

(defun CLEAR-ALL-METERS ()			; This would presumably get called at the beginning of a run.
  (loop for (scalar-symbol init-form) in *scalar-metering-counters*
	do (set scalar-symbol (eval init-form)))
  (loop for (vector-symbol init-form) in *vector-metering-counters*
	do (set vector-symbol (eval init-form))))

(defmacro-definer DEF-SCALAR-METERING-COUNTER (name &optional (init-form 0))	; We'll assume the counter starts from zero, but allow revision.
  `(progn
     (error-if-already-defined-elsewhere ',name '*scalar-metering-counters*)
     (pushnew-car-replace (list ',name ',init-form) *scalar-metering-counters*)
     (defvar ,name (eval ,init-form))))

(defmacro-definer DEF-SCALAR-METERING-COUNTER-PERIODICALLY-REPORTED (name label &optional (init-form 0))
  `(progn
     (pushnew-car-replace (list ',name ,label) *scalar-metering-counters-periodically-reported*)
     (def-scalar-metering-counter ,name ,init-form)))

(defmacro-definer DEF-VECTOR-METERING-COUNTER (name &optional (init-form nil))	 ; We'll assume the vector starts off empty, but allow revision.
  `(progn
     (error-if-already-defined-elsewhere ',name '*vector-metering-counters*)
     (pushnew-car-replace (list ',name ',init-form) *vector-metering-counters*)
     (defvar ,name nil)))

(defmacro-definer DEF-VECTOR-METERING-COUNTER-PERIODICALLY-REPORTED (name label &optional (init-form nil))
  `(progn
     (pushnew-car-replace (list ',name ,label) *vector-metering-counters-periodically-reported*)
     (def-vector-metering-counter ,name ,init-form)))

;;; +++ "Parametric-info".

;;; Yeah, yeah, I should either be using generic method dispatch, or at least define a macro
;;; used to define such symbols so that the appropriate entry gets made on this alist.  Later.
;;; Note also that all of the variables mentioned here are actually forward references, since
;;; the variables that are going into this bag aren't defined until after here.
(defparameter *PARAMETRIC-INFO-SYMBOLS* '((*iterator-generators-in-use* iteration-generators-in-use-parametric-snapshot)
					  (*reported-parameters*        reported-parameters-parametric-snapshot)))

;;; NOTE that half of the point of this thing is not only to save parameter settings in snapshots
;;; (so I can unambiguously figure out what they were later), but to NOT have those saved settings
;;; smash the current settings in the running world, which were probably compiled in somewhere.
;;; So nothing that gets snapshotted here ever gets restored; it's just there for debugging purposes,
;;; so I can tell the state of the world when it was dumped.
(defun SNAPSHOT-PARAMETRIC-INFO ()
  ;; The two LISTs below are so that, when we append this in with all the other metering info,
  ;; all of this still is "one list".  It also guarantees that the entry will be a list, not a symbol,
  ;; hence it'll never be "known" as a metering variable in any way.
  (list (list (list '*parametric-info-symbols*
		    (loop for (parametric-symbol parametric-value-fn) in *parametric-info-symbols*
			  collect (list parametric-symbol
					(funcall parametric-value-fn))))
	      ;; Theoretically, since RESTORE-METERING-INFO only restores meters whose names it
	      ;; knows, it'll never actually try to set *PARAMETERIC-INFO-SYMBOLS* to anything.
	      ;; However, we still have to ensure that the overall list has an even number of elements,
	      ;; hence we might as well put the real value in here, in case something _does_ use it...
	      *parametric-info-symbols*)))

(defun PARAMETRIC-INFO-FROM-SNAPSHOT (snapshot-spec)
  (let ((metering-info (metering-info-from-snapshot snapshot-spec)))
    (assoc '*parametric-info-symbols* metering-info
	   :test #'(lambda (name alist-entry)
		     (when (consp alist-entry)
		       (eql (car alist-entry) name))))))

;;; This is about as specific as one can get, since the different types (e.g.,
;;; *REPORTED-PARAMETERS*, *ITERATOR-GENERATORS-IN-USE*) store their info in
;;; different ways.
(defun PARTICULAR-PARAMETRIC-INFO-FROM-SNAPSHOT (snapshot-spec type)
  (let* ((all-info (parametric-info-from-snapshot snapshot-spec))
	 (useful-info (second (car all-info))))
    (second (assoc type useful-info))))

;;; +++ "Limits".  (See the error-checker above for the definition of the accumulating variable.)

;;; We expect microworlds to use some of the stuff below, so the useful stuff is exported.

(defun ALL-LIMIT-NAMES ()
  (mapcar #'car *limits*))

(def-debugging-switch WARN-FOR-UNSAVED-LIMIT t)	; Some limit we know about wasn't saved in this snapshot.
(def-debugging-switch WARN-FOR-CHANGED-LIMIT t)	; Some limit is about to be changed to a non-EQL value as a result ...
						; ... of reloading (either from init or unsaved-default).
(def-debugging-switch WARN-FOR-PROPAGATED-CHANGED-LIMIT t)	; Let us know if changing one thing changes something else.

;;; Note that it is a _bad_ idea for the init-form or the unsaved-default-form
;;; to have side-effects.  We'll evaluate the init-form for _every_ DEFLIMIT
;;; every time a DEFLIMIT is defined or redefined (yes, this is O(n^2)), since
;;; some DEFLIMITs may be derived from others, and doing so will update them
;;; as well.  Note also that this means that having a circular dependency is also
;;; probably a bad idea, and we don't check for this, nor do we attempt to make
;;; any of this more efficient by doing constraint propagation.  Note finally that
;;; we use EQUALP to tell if something changed, and hence whether to continue
;;; propagating changes.  (This means that things that cons strings will appear
;;; the same if the strings are STRING-EQUAL, which is probably what you wanted.)
(defun PROPAGATE-NEW-LIMITS (original-name)
  (loop with changed-something = t		; Keep going until nothing has changed.
	while changed-something
	do (setf changed-something nil)
	   (loop for (name init-form unsaved-default-form) in *limits*
		 do (ignore unsaved-default-form)
		    (let ((current-value (symbol-value name))
			  (new-value (eval init-form)))
		      (unless (equalp current-value new-value)
			(when *warn-for-propagated-changed-limit-enabled*	; We're using WARN below, so we shouldn't use the -FORMAT macro.
			  (warn "As a result of propagating a change to the value of ~S ~S,~&~
                                 the value of ~S ~S is being changed from ~S to ~S."
				'deflimit original-name
				'deflimit name
				current-value new-value))
			(set name new-value)
			(setf changed-something t)))))
  (values))

;;; [&&& The DEFPROP below doesn't work.  The DEFPARAMETER inside this form
;;; already calls SCL:RECORD-SOURCE-FILE-NAME correctly.  Yet Meta-. can't find
;;; things defined with DEFLIMIT.  I dunno what's wrong.  It probably isn't Zwei
;;; sectionization, because Meta-. doesn't even have a clue about what file it's in.
;;; See SYS:COLOR;GENEX;DEF-DEFINING-FORM.LISP for more possible clues.]
; (scl:defprop  deflimit "Defining Form" si:definition-type-name)

;;; UNSAVED-DEFAULT is the default value to which we must set this limit
;;; if we restore from a snapshot which didn't set it at all (e.g., some snapsnot
;;; taken either before this whole mechanism was in place, or before some
;;; particular limit was established).
;;;
;;; Note that the ERROR-CHECKING-FORM is presumably some ASSERT form which will
;;; fail if, for example, a limit is set wrong.  We evaluate it _after_ doing the internal
;;; DEFLIMIT (since otherwise we really don't have an appropriate scope for the
;;; form to use in checking anything---the limit variable isn't set yet, and the
;;; INIT-FORM isn't easy to get to here), which means that, if the assertion blows
;;; out, the limit has _already_ been changed (possibly propagating changes to other
;;; limits, too).  This shouldn't be a problem if limit initialization forms don't have
;;; side-effects (which it is documented that they shouldn't), and if you eventually
;;; fix whatever problem caused the assertion to blow out and reevaluate the
;;; DEFLIMIT form (which you presumably will, since it blew out in the middle of
;;; compiling it).
;;;
;;; Note that the ERROR-CHECKING-FORM is evaluated _after propagating changes_ to
;;; other limits, for the following reason:  We certainly can't evaluate it before the
;;; internal DEFLIMIT (see discussion immediately above).  We _could_ evaluate it
;;; before propagating limits, but the assertion itself might depend on seeing a
;;; self-consistent set of limits (not just the limit mentioned explicitly in this call to
;;; DEFLIMIT), and that would cause it to fail when ordinarily things are copacetic.  If
;;; we evaluate it after limit propagation, then we can check all the relevant limits at
;;; once, and do the right thing (in essence, the internal limit change and the
;;; propagation are treated an a single, atomic action).  If it _then_ blows out, after
;;; propagation, well, the blowout will have to be fixed at _some_ point anyway
;;; (since it blew out in the middle of compiling the DEFLIMIT), and whatever
;;; recompilation of the deflimit will again do propagation, presumably fixing the
;;; damage.
;;;
;;; This is the routine that actually does the work.  It's split out this way because
;;; occasionally we have a caller (e.g., RESTORE-LIMIT-INFO) that has to do most of
;;; the work, but in a slightly different way.
(defmacro WITH-DEFLIMIT-INTERNAL ((&body body) name init-form unsaved-default-form &optional error-checking-form)
  `(progn
     (error-if-already-defined-elsewhere ,name '*limits*)
     (pushnew-car-replace (list ,name ,init-form ,unsaved-default-form) *limits*)
     ,@body
     (propagate-new-limits ,name)
     ,(when error-checking-form
	error-checking-form)
     ))

;;; This is the exported way to do it, and what you'd put at toplevel in a file.
(defmacro-exported-definer DEFLIMIT (name init-form unsaved-default-form &optional error-checking-form)
  `(with-deflimit-internal
     ((defparameter ,name (eval ,init-form)))
     ',name ',init-form ',unsaved-default-form ,error-checking-form))

(defsubst-exported LIMIT-VALUES (name)
  (unless (boundp name)
    (error "~S ~S doesn't appear to be bound." 'deflimit name))
  (let ((entry (find name *limits* :key #'car)))
    (unless entry
      (error "Can't find ~S ~S in the defined ~2:*~Ss." 'deflimit name))
    (let ((current (symbol-value name))
	  (historical (eval (third entry))))
      (values current historical))))

;;; Handy for things that haven't been updated yet, and know that they will break if
;;; called for some DEFLIMIT value that's different than the historical setting.
;;; Essentially for debugging.  Note that, since this has to call EVAL at least once, not
;;; to mention searching a list, it's _slow_ and should _not_ be put inside any loops!
;;; If you call this with something that isn't a limit, it'll blow out informatively.
(defun-exported DEFLIMIT-NOT-HISTORICAL (name)
  (multiple-value-bind (current historical)
      (limit-values name)
    (not (equalp current historical))))

;;; Same efficiency caveats about DEFLIMIT-NOT-HISTORICAL apply to this macro...
(defmacro-exported WHEN-DEFLIMIT-NOT-HISTORICAL (name &body body)
  `(when (deflimit-not-historical ,name)
     ,@body))

(defmacro-exported ERR-IF-DEFLIMITS-NOT-HISTORICAL (&rest names)
  `(let ((errs (remove-if-not #'deflimit-not-historical `(,',@names))))
     (when errs
       (error "The following ~S~P ~:[are~;is~] not at ~:*~:[their~;its~] historical setting,~&~
               but ~:*~:[are~;is~] supposed to be:  ~A"
	      'deflimit
	      (length errs)
	      (= (length errs) 1)
	      (format-print-list nil "~S" errs)))))

(defun SNAPSHOT-LIMIT-INFO ()
  (loop for (name init-form unsaved-default-form) in *limits*
	do (ignore unsaved-default-form)
	collect (list name init-form)))

;;; Note that we MUST propagate new limits.  This would seem to be unnecessary, since presumably
;;; any limits that changed because of some other limit would be dumped in their changed form and then
;;; restored correctly, but this assumption breaks in the face of two facts:
;;;   A limit is not necessarily just a constant---it can be a form to eval, and
;;;   There is no guarantee that we restore limits in the correct order for such eval'ed init-forms
;;;   to be set correctly.  In fact, we tend to restore them in the reverse order, because newer
;;;   DEFLIMITS (which tend to be defined in terms of older ones) get pushed onto the front of the list.
;;;   That means that the newer ones get restored first.  Then, when the older ones (upon which they
;;;   depend) get restored, the correct propagation hasn't happened.
;;; So we force propagation to happen for each limit we restore, guaranteeing that everything gets
;;; correctly reset.  (This includes limits that are "implicitly" restored, by virtue of not having been
;;; mentioned in the snapshot at all.  I suppose that this could be a problem if such a limit also depended
;;; on something else, since we reset it back, but presumably in such a case propagation after the resetting
;;; will set it back correctly again, despite our attempt to implicitly reset it.)  Note that all of this work
;;; is yet another reason why init-forms with side-effects, or circular dependencies, are a bad idea.
(defun RESTORE-LIMIT-INFO (info)
  (labels ((maybe-warn-about-change (symbol new-value)
	     (unless (eql (symbol-value symbol) new-value)
	       (warn-for-changed-limit-format
		 "~&Limit ~S, being restored from a snapshot,~&~
                  is having its value changed from ~S to ~S.~&"
		 symbol
		 (symbol-value symbol)
		 new-value)))
	   (default-init-form-and-value (symbol)
	     ;; The lookup below must succeed, since the only names we're looking for came originally from the contents
	     ;; of *LIMITS*, which is the list we're searching.  But be paranoid anyway.
	     #+Genera (declare (values default-init-form default-init-value))
	     (let ((restoration-info (find symbol *limits* :key #'car)))
	       (unless restoration-info
		 (error "Can't have gotten here!"))
	       (values (third restoration-info)
		       (eval (third restoration-info)))))
	   (re-deflimit (symbol init-form)	; No error-checking-form, because it's not saved.
	     (let ((unsaved-default-init-form (default-init-form-and-value symbol)))	; Don't need the value here.
	       (with-deflimit-internal
		 ((set symbol (eval init-form)))
		 symbol init-form unsaved-default-init-form))))
    (let ((restored-limits nil)
	  (all-limit-names (all-limit-names)))
      (loop for (symbol init-form) in info
	    when (member symbol *limits* :key #'car)	; If we've never heard of it, just ignore it (e.g., it's the name of a meter, not a limit).
	      do (maybe-warn-about-change symbol (eval init-form))
		 (re-deflimit symbol init-form)
		 (push symbol restored-limits))
      ;; Since we only restore a limit if we know about it (e.g., it's in ALL-LIMIT-NAMES), SET-DIFFERENCE, and not INTERSECTION, is fine.
      (let ((unrestored (set-difference all-limit-names restored-limits)))
	(loop for name in unrestored
	      do (multiple-value-bind (default-init-form default-init-value)
		     (default-init-form-and-value name)
		   (warn-for-unsaved-limit-format
		     "~&Limit ~S was not originally saved in this snapshot.~&~
                        Its value is being set to its default, ~S.~&"
		     name default-init-value)
		   (maybe-warn-about-change name default-init-value)
		   (re-deflimit name default-init-form))))))
  (values))

;;; +++ Actually dumping and reloading all of the above.

(defun SNAPSHOT-METERING-INFO ()
  (append
    (loop for (scalar-symbol) in *scalar-metering-counters*
	  collect (list scalar-symbol
			(symbol-value scalar-symbol)))
    (loop for (vector-symbol) in *vector-metering-counters*
 	  collect (list vector-symbol
			(copy-seq (symbol-value vector-symbol))))))

(defun SNAPSHOT-ANCILLARY-INFO ()
  (append
    (snapshot-metering-info)
    (snapshot-limit-info)
    (snapshot-parametric-info)
    ))

(defun RESTORE-METERING-INFO (info)
  (let ((all-known-counters (append *scalar-metering-counters* *vector-metering-counters*)))
    (loop for (symbol value) in info
	  for counter-is-sequence? = (member symbol *vector-metering-counters*)
	  when (member symbol all-known-counters :key #'car)	; If we've never heard of it, just ignore it.
	  do (set symbol
		  (if counter-is-sequence?
		      (copy-seq value)
		      value))))
  (values))

(defun RESTORE-ANCILLARY-INFO (info)
  (restore-metering-info info)
  (restore-limit-info info)
  (values))

;;; These two trivial functions are a good argument for using CLOS method dispatch on the arg type.
;;; The peculiar handling of BELOW (e.g., not making it HIGHEST instead) is because it's more likely
;;; that I'll call 'em with, e.g., *ITEM-NUMBER*, which points one past the highest active number. 
(defun LONGEST-PRINTED-REP-VECTOR (what &optional (accessor #'identity)
				   (below (1+ (length what))))	; Avoid having to cons up a list for this case.
  (loop for thing being the array-elements of what
	repeat (1- below)
	maximize (length (funcall accessor thing))))

(defun LONGEST-PRINTED-REP (what &optional (accessor #'identity)
			    (below (1+ (length what))))
  (loop for thing in what
	repeat (1- below)
	maximize (length (funcall accessor thing))))

(defun LONGEST-METER-PRINTED-REP ()
  ;; For poor-man's table formatting.  Only for debugging at the moment.
  (longest-printed-rep (all-meter-names) #'symbol-name))

;;; Assumes that, if the first element of a vector is a number, they all are.
;;; This assumption can get fixed if I ever make a meter that doesn't obey this convention.
(defun SUMMARIZE-METERS (&optional (stream t))	; A debugging function.
  (let ((max-name-length (longest-meter-printed-rep)))
    (flet ((format-name (symbol)
	     (safe-format stream "~&~V@<~S~> "
			  max-name-length
			  symbol)))
      (loop for (scalar-symbol) in *scalar-metering-counters*
	    for scalar = (symbol-value scalar-symbol)
	    do (format-name scalar-symbol)
	       (format stream "= ~S~&"
		       scalar))
      (loop for (vector-symbol) in *vector-metering-counters*
	    for vector = (symbol-value vector-symbol)
	    do (let ((length (length vector)))
		 (format-name vector-symbol)
		 (cond ((zerop length)
			(format stream "is empty.~&"
				vector-symbol))
		       (t
			(format t "(length ~:D) "
				length)
			(cond ((consp vector)
			       (cond ((numberp (car vector))
				      (format stream "sums to ~:D.~&"
					      (reduce #'+ vector)))
				     (t
				      (format stream "has nonnumeric elements.~&"))))
			      (t
			       #-Genera (format t "isn't a list.~&")
			       #+Genera (format t "is ~\\a-or-an\\.~&"
						(zl:typep vector))))))))))
  (values))

(defun SUM-SINGLE-VECTOR-METER (name snapshot)
  (reduce #'+ (named-meter-from-snapshot name snapshot)))

;;; Some convenience functions.
(defun REPORT-SINGLE-VECTOR-METER (snapshot name)
  (format nil "~S sums to ~:D"
	  name
	  (sum-single-vector-meter name snapshot)))

(defun MAKE-PREFIX-FOR-SYMBOL (old-symbol prefix)
  ;; If the first character of the old symbol's name is a *, puts the prefix after that.
  ;; Otherwise, puts the prefix first.
  (when (or (null prefix)
	    (zerop (length prefix)))
    (setf prefix ""))
  (when (and (not (zerop (length prefix)))
	     (not (char-equal (aref prefix (1- (length prefix))) #\-)))
    (setf prefix (format nil "~A-" prefix)))
  (let ((old-name (symbol-name old-symbol)))
    (cond ((char-equal (aref old-name 0) #\*)
	   (format nil "~:@(*~A~A~)"
		   prefix
		   (subseq old-name 1)))
	  (t
	   (format nil "~:@(~A~A~)"
		   prefix
		   old-name)))))

;;; This MUST BE KEPT IN SYNC with SNAPSHOT-WORLD-STATE!
;;; [That function is defined in a much later file than this macro, because that's where snapshotting is defined.]
(defmacro WITH-SNAPSHOT-DESTRUCTURING (snapshot &body body)
  `(destructuring-bind ((schema-system-version
			 timestamp clock-tick comment
			 &optional metering-state)	; &optional because, prior to Schema 6.7, we didn't store metering results.
			random-state schema-state microworld-state)
       ,snapshot
     ,@body))

;;; This MUST BE KEPT IN SYNC with SNAPSHOT-WORLD-STATE!
;;; [That function is defined in a much later file than this macro, because that's where snapshotting is defined.]
(defmacro WITH-COMPLETE-SNAPSHOT-DESTRUCTURING (snapshot &body body)
  `(destructuring-bind (((major-version minor-version status system system-branch eco-level)
			 timestamp clock-tick comment
			 &optional metering-state)	; &optional because, prior to Schema 6.7, we didn't store metering results.
			random-state schema-state microworld-state)
       ,snapshot
     ,@body))

(defun COMMENT-INFO-FROM-SNAPSHOT (&optional snapshot-spec)
  (let ((snapshot (snapshot-from-spec snapshot-spec)))
    (with-snapshot-destructuring snapshot
      schema-system-version timestamp clock-tick metering-state schema-state random-state microworld-state	; Ignored.
      comment)))

(defun METERING-INFO-FROM-SNAPSHOT (&optional snapshot-spec)
  (let ((snapshot (snapshot-from-spec snapshot-spec)))
    (with-snapshot-destructuring snapshot
      schema-system-version timestamp clock-tick comment schema-state random-state microworld-state	; Ignored.
      metering-state)))

(defun NAMED-METER-FROM-SNAPSHOT (name &optional snapshot-spec)
  (let ((metering-info (metering-info-from-snapshot snapshot-spec)))
    (second (assoc name metering-info))))

(defun METER-NAMES-IN-SNAPSHOT (&optional snapshot-spec)
  (let ((metering-info (metering-info-from-snapshot snapshot-spec)))
    (mapcar #'car metering-info)))

;;;; Stopping execution at a particular point.

(defparameter *ITERATION-EXECUTE-WHEN* nil)	; Iteration, item, schema.

(defun-exported ITERATION-EXECUTE-WHEN (iteration item schema continuation &rest args)
  (when (and *iteration-execute-when*			; Paranoia...
	     (consp *iteration-execute-when*)		; ...
	     (= (length *iteration-execute-when*) 3)	; ...
	     (destructuring-bind (iteration-1 item-1 schema-1)	; Real testing...
		 *iteration-execute-when*
	       (and (= iteration iteration-1)
		    (= item item-1)
		    (= schema schema-1))))
    (apply continuation args)))

(defun-exported ITERATION-BREAK-WHEN (iteration item schema)
  (iteration-execute-when iteration item schema #'break))

;;;; Variables that work kinda like meters, but they're not reported, nor are they saved.
;;;; They _are_, however, cleared at the beginning of a new run.  This exists just to track
;;;; them and make that easier.  (These variables are, by definition, part of the schema
;;;; mechanism, not the microworld, and maybe should be incorporated elsewhere in the
;;;; data structures, except that this mechanism doesn't require recompiling defstructs.)
;;;; (This kinda also like defvar-resettable, now that I think about it...)

(defvar *RUN-VARIABLES* nil)

(def-microworld-independent-init CLEAR-ALL-RUN-VARIABLES ()
  (loop for (symbol init-form) in *run-variables*
	do (set symbol (eval init-form))))

(defmacro-exported-definer DEF-RUN-VARIABLE (name init-form)
  `(progn
     (defvar ,name (eval ,init-form))
     (pushnew-car-replace (list ',name ',init-form) *run-variables*)))

;;;; Parameters that get reported at the start of a run, in the way that generators do.

(defvar *REPORTED-PARAMETERS* nil)

;;; ERROR-CHECKING-FORM should be some assertion, and is evaluated _after_ changing
;;; the parameter, for reasons given above in DEFLIMIT (having to do making the variable
;;; accessible to the checking-form, and the need to recompile after a blowout anyway).
(defmacro-exported-definer DEF-REPORTED-PARAMETER (name init-form &optional error-checking-form)
  `(progn
     (defparameter ,name ,init-form)
     (pushnew-car-replace (list ',name ',init-form) *reported-parameters*)
     ,(when error-checking-form
	error-checking-form)))

(defun REPORTED-PARAMETERS-PARAMETRIC-SNAPSHOT ()
  *reported-parameters*)

(defun REPORTED-PARAMETER-FROM-SNAPSHOT (snapshot parameter)
  (second (assoc parameter
		 (particular-parametric-info-from-snapshot
		   snapshot '*reported-parameters*))))

;;;; A stripped-down, somewhat specialized, but portable version of a concept
;;;; implemented at least twice already in Genera:  SYS:DEFSTORAGE and
;;;; LMFS:DEFSTORAGE.  Reimplemented from scratch, though, because those two are
;;;; too general and too dependent upon Genera innards.

;;; OBJECT-NAME provides the first part of the names of all of the generated macros.
;;; Each FIELD in FIELDS is a doublet of the name of the field and its size.  The offsets
;;; are generated automatically, which means that the fields must be in storage order.
;;; Note that this disallows overlaps in the fields.  The size may be a form as well as
;;; an ordinary integer; in this case, it _must_ evaluate to an integer at compile-time.
;;; It had better not have side-effects, because we don't guarantee not to evaluate it
;;; multiple times (in particular, we evaluate it at least once for error-checking
;;; before using it for real; this could be fixed if I _had_ to, but there seems to be
;;; no point to fixing it right now).
;;;
;;; This macro is somewhat specialized.  It builds two macros for each field.  The
;;; different macros each take an arg which is:
;;;   an instance of the object, or
;;;   the data slot from an instance of the object (the slot must be named DATA).
;;; Each of these two macros then also has a macro built which is a predicate,
;;; applying FLAG-TRUEP to its result.
;;;
;;; We also define a field offset for each field, which indicates at what position
;;; the field starts.  Its name is "%%" plus the name of the object and the field,
;;; and the string "-offset". We do _not_ check for accidental duplications!
;;; (There shouldn't _be_ any, but...)  We also define similar "-size" entries.
;;; 
;;; &&& Note that SCL:RECORD-SOURCE-FILE-NAME and/or SYS:MULTIPLE-DEFINITION
;;; &&& don't appear to help Zwei in finding where in the buffer anything defined
;;; &&& in this fashion actually is.  Meta-. at least knows the file, but that's it.  *sigh*
;;;
;;; As of Schema 11.8 and all earlier versions, nobody outside of the schema mechanism
;;; itself uses this, but I'm exporting it anyway, because it looks useful.  Its two ancillary
;;; functions immediately below don't look all that useful to outsiders, though, so they stay
;;; private.
(defmacro-exported-definer DEFSTORAGE (object-name &body fields)
  ;; Do some easy error-checks up front.
  (unless fields
    (error "No fields specified."))
  (mapc #'(lambda (field)
	    (unless (and (= (length field) 2)
			 (symbolp (first field))
			 (integerp (eval (second field))))
	      (error "Malformed field:  ~S" field)))
	fields)
  ;; Now build all the forms.
  (let ((forms
	  (loop with offset = 0
		;; Compensate for an UNBELIEVABLE bug I tripped over.  The definition of SCHEMA's DATA field is
		;; _one bit too long_, in the original code.  Perhaps Ramstad was incorrect about Lucid having
		;; 24-bit fixna, but it couldn't possibly have worked in that implementation if he was correct.
		;; So allow the use of a longer fixnum for this, because, after all, it _does_ work in MCL and
		;; Genera anyway, and _might have_ worked in Lucid.  Since I'm not sure about the Lucid case,
		;; I'll be maximally conservative there, and let some Lucid user figure out the true state of affairs.
		with fixnum-length =
		  #-Lucid (number-of-bits-in-a-fixnum)
		  #+Lucid *safe-fixnum-length*
		with accessor = (intern-format "~A-data" object-name)
		for (field-name size-form) in fields
		for size = (eval size-form)
		for field-offset-name = (intern-format "%%~A-~A-offset" object-name field-name)
		for field-size-name   = (intern-format "%%~A-~A-size"   object-name field-name)
		for data-macro-name   = (intern-format "~A-data-~A"     object-name field-name)
		for inst-macro-name   = (intern-format "~A-~A"          object-name field-name)
		for data-pred-name    = (intern-format "~A-p"           data-macro-name)
		for inst-pred-name    = (intern-format "~A-p"           inst-macro-name)
		;; Logically, this WHEN belongs right after the computation of SIZE, and Genera allows it there.
		;; However, Harlequin insists that iterator clauses must come before the main body of the loop.
		when (> (+ offset size) fixnum-length)
		  do (error "Field ~S of object ~S, which starts at offset ~D,~&~
                             is ~D bit~:P long, and sticks off the end of a fixnum~&~
                             (length ~D).  There are just too many bits worth of~&~
                             fields in this object for a single fixnum.~&"
			    field-name object-name offset size fixnum-length)
		append `((eval-when (load eval compile)	; These values may be needed later in the same file for compiling other macros...
			   (defconstant ,field-offset-name ,offset)
			   (defconstant ,field-size-name   ,size)
			   )			; EVAL-WHEN
			 (defmacro ,data-macro-name (data)
			   `(ldb (byte ,',size ,',offset) ,data))
			 (defmacro ,inst-macro-name (,object-name)
			   `(,',data-macro-name (,',accessor ,,object-name)))
			 (defmacro ,data-pred-name (data)
			   `(flag-truep (,',data-macro-name ,data)))
			 (defmacro ,inst-pred-name (,object-name)
			   `(,',data-pred-name (,',accessor ,,object-name))))
		do (incf offset size))))
    ;; Finally, emit them.
    `(progn
       ,@forms)))

;;; Used by folks once the fields are defined. 
(defmacro-definer DEF-DATA-AND-INSTANCE-MACROS (object-name (macro-name-suffix &rest extra-args) fn offset)
  (let ((data-macro-name (intern-format "~A-data-~A" object-name macro-name-suffix))
	(inst-macro-name (intern-format "~A-~A"      object-name macro-name-suffix))
	(accessor        (intern-format "~A-data"    object-name)))
    `(progn
       (defmacro ,data-macro-name (data ,@extra-args)
	 `(,',fn ,data ,,offset ,,@extra-args))
       (defmacro ,inst-macro-name (,object-name ,@extra-args)
	 `(,',data-macro-name (,',accessor ,,object-name) ,,@extra-args)))))

(defmacro-definer DEF-DATA-AND-INSTANCE-MACROS-2 (object-name (macro-name-suffix) fn offset)
  (let ((data-macro-name (intern-format "~A-data-~A" object-name macro-name-suffix))
	(inst-macro-name (intern-format "~A-~A"      object-name macro-name-suffix))
	(accessor        (intern-format "~A-data"    object-name))
	(data-arg1       (intern-format "data1"))
	(data-arg2       (intern-format "data2"))
	(inst-arg1       (intern-format "~A1" object-name))
	(inst-arg2       (intern-format "~A2" object-name)))
    `(progn
       (defmacro ,data-macro-name (,data-arg1 ,data-arg2)
	 `(,',fn
	   ,,data-arg1 ,,offset
	   ,,data-arg2 ,,offset))
       (defmacro ,inst-macro-name (,inst-arg1 ,inst-arg2)
	 `(,',data-macro-name
	   (,',accessor ,,inst-arg1)
	   (,',accessor ,,inst-arg2))))))

;;; For cases involving SETF of LDB in which we're generating new numbers out of
;;; whole cloth.  We need a generalized reference, but we don't care about it.
(defmacro WITH-GENERALIZED-REFERENCE (name &body body)
  `(let ((,name 0))
     ,@body
     ,name))

;;;; Making it easy for microworlds to build objects.

;;; Since these are designed for microworlds to call, and since microworlds are all
;;; supposed to be in their own packages, we export them make it easy for
;;; microworlds to use them.

;;; Call this in the correct package for the given microworld.  It creates the
;;; necessary global variable and macros in the correct package so that they
;;; may be used locally, and (as long as you don't put more than one microworld
;;; in a given package) they will stay separate from other microworlds.
;;;
;;; This macro defines two global variables, three macros, and a function, all in the
;;; package associated with its arg NAME (nothing but NAME's package is relevant):
;;; *PRIMITIVE-ITEMS*, *NUMBER-OF-PRIMITIVE-ITEMS*, DEFITEM, DEFITEM-BUILT,
;;; DEF-SIMPLE-ITEM-NAMING-FUNCTION, and MAKE-ALL-ITEMS.  [Note that the
;;; MAKE-ALL-ITEMS in the microworld's package calls SCHEMA::MAKE-ALL-ITEMS, which
;;; isn't defined until late in SCHEMA-MECHANISM.LISP.  Not to worry.]
;;;
;;; Note that *NUMBER-OF-PRIMITIVE-ITEMS* is not a limit:  if there is 1 item, this
;;; value is 1, not 2.  It is therefore equivalent to (LENGTH *PRIMITIVE-ITEMS*), and
;;; is set by MAKE-ALL-ITEMS (the one in the microworld's package!), when it runs.
(defmacro-exported-definer DEF-DEFITEM (name)
  (let ((pkg (symbol-package name)))
    (let ((global        (intern-format-pkg pkg "*primitive-items*"))
	  (global-number (intern-format-pkg pkg "*number-of-primitive-items*"))
	  (defitem       (intern-format-pkg pkg "defitem"))
	  (defitem-built (intern-format-pkg pkg "defitem-built"))
	  (def-sinf      (intern-format-pkg pkg "def-simple-item-naming-function"))
	  (make-all      (intern-format-pkg pkg "make-all-items")))
      `(progn
	 (defvar ,global nil)
	 (defvar ,global-number nil)		; Filled in later by MAKE-ALL-ITEMS, when it runs.
	 (defmacro ,defitem (name &body body)
	   `(progn
	      (pushnew ',name ,',global)
	      (defun-exported ,name ()
		,@body)))
	 (defmacro ,defitem-built (item-fn naming-fn &rest args)
	   (let ((name (apply naming-fn (car args) (cdr args))))
	     `(,',defitem ,name
	       (,item-fn ,@args))))
	 (defmacro ,def-sinf (base-name)
	   (let ((fn-name (intern-up (format nil "simple-item-naming-function-~A" base-name) ,pkg)))
	     `(eval-when (load eval compile)	; We'll need this function to help expand the other macros we're building here...
		(defun ,fn-name (&rest args)
		  (intern-up
		    (format nil "~A~A"
			    ',base-name
			    (reduce #'string-append
				    (mapcar #'(lambda (arg)	; I want PRINC-TO-STRING or somesuch.
						(format nil "~A" arg))
					    args)))
		    ,,pkg)))))
	 (defun ,make-all (the-pkg)
	   (setf ,global-number (make-all-items ,global the-pkg)))
	 ))))
	    
;;; Quite like DEF-DEFITEM above, but very much simpler, because there's no great
;;; regularity in actions and hence no need to automatically generate one- or
;;; two-dimensional arrays of them.
(defmacro-exported-definer DEF-DEFACTION (name)
  (let ((pkg (symbol-package name)))
    (let ((global        (intern-format-pkg pkg "*actions*"))
	  (global-number (intern-format-pkg pkg "*number-of-actions*"))	; Not a limit:  if there is 1 action, this value is 1, not 2.
	  (defaction     (intern-format-pkg pkg "defaction"))
	  (make-all      (intern-format-pkg pkg "make-all-actions")))
      `(progn
	 (defvar ,global nil)
	 (defvar ,global-number nil)		; Filled in later by MAKE-ALL-ACTIONS, when it runs.
	 (defmacro ,defaction (name &body body)
	   `(progn
	      (pushnew ',name ,',global)
	      (defun-exported ,name ()
		,@body)))
	 (defun ,make-all (the-pkg)
	   (setf ,global-number (make-all-actions ,global the-pkg)))
	 ))))

(defvar *ALL-MICROWORLDS* nil)

;;; Calls the above two macros to generate lots of code we'll need.  Also produces a
;;; function called INITIALIZE-SCHEMA-MECHANISM, in the package of the microworld,
;;; which should be called from whatever more-specific microworld-initialization
;;; function exists.
(defmacro-exported-definer DEF-MICROWORLD (name)
  (let ((pkg (symbol-package name)))
    (let ((initializer (intern-format-pkg pkg "initialize-schema-mechanism"))
	  (all-items   (intern-format-pkg pkg "make-all-items"))
	  (all-actions (intern-format-pkg pkg "make-all-actions")))
      `(progn
	 (pushnew ',name *all-microworlds*)
	 (def-defitem ,name)
	 (def-defaction ,name)
	 (defun ,initializer ()
	   (clear-some-mechanism-counters)
	   (,all-items ,pkg)
	   (,all-actions ,pkg)
	   (values))))))

;;; End of file.
