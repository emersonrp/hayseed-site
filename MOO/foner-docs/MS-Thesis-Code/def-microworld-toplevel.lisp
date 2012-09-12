;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

(in-package :schema)

;;;; Generic toplevel for running various microworlds.

;;;; Some metering support.

(defvar *ITERATION-METERING-COUNTDOWN* nil)	; Meter the following n clock ticks.
(defvar *ITERATION-METERING-TRIGGER* nil)	; When the clock equals the CAR, meter the next CADR clock ticks.
(defparameter *BEEP-WHEN-COUNTDOWN-COMPLETE* t)	; Whether to beep when we finish a metering run.

(defmacro METER-ZERO? ()
  `(or (null *iteration-metering-countdown*)
	     (zerop *iteration-metering-countdown*)))

(defmacro DELAYED-TRIGGER-VALID? ()
  `(and *iteration-metering-trigger*
	(consp *iteration-metering-trigger*)
	(numberp (car *iteration-metering-trigger*))
	(numberp (second *iteration-metering-trigger*))))

(defun MAYBE-BEEP-UPON-COMPLETION ()
  (when (and *beep-when-countdown-complete*
	     (meter-zero?))
    #+Genera (sys:beep)))			; Every Lisp's different.  But metering only makes sense for Genera anyway.

(defun MI-ENABLED? ()
  ;; Keeping me from screwing up...
  #-Genera nil
  #+Genera #+Genera
  (declare (special mi::with-metering-enabled-trigger))
  (zl:symeval-in-instance mi::with-metering-enabled-trigger 'metering:active-region))	; Ewww...

(defun WARN-IF-MI-DISABLED ()
  (unless (mi-enabled?)
    (format t "~&Warning:  The metering interface is not currently waiting to start metering!~&")
    t))						; Let our caller add to this message...

;;; Simple setters for the above...
(defun METER-NEXT (&optional (how-many 1))
  (cond ((warn-if-mi-disabled)
	 ;; Chances are, another iteration will have happened by the time I can possibly have
	 ;; turned the metering substrate on, hence our counter will have ticked down at least
	 ;; one tick and probably to zero if it was small.  So don't even bother; I'll have to turn
	 ;; on metering and try again anyway.
	 (format t "Iteration meter setup aborted.~&"))
	(t
	 (cond ((meter-zero?)
		(setf *iteration-metering-countdown* how-many))
	       (t
		(format t "~&Sorry, already counting down (currently at ~A).~&"	; Ignore possible timing screw with test vs later format...
			*iteration-metering-countdown*)))))
  (values))

(defun METER-AT (clock-tick &optional (how-many 1))
  (when (warn-if-mi-disabled)
    ;; Chances are, this is scheduled for a ways in the future, so I'll have plenty of time
    ;; to turn on the metering substrate, so do the setting anyway and hope I remember to
    ;; turn on the substrate in time.
    (format t "Iteration meters set up anyway.~&"))
  (cond ((> clock-tick *clock-tick*)
	 (setf *iteration-metering-trigger* (list clock-tick how-many)))
	(t
	 (format t "~&Sorry, the clock is already at ~A.~&"
		 *clock-tick*)))
  (values))

(defun METERING-STATUS ()
  (if (meter-zero?)
      (format t "~&Metering is not currently in progress, ~
                 ~:[and the metering substrate is disabled~
                  ~;but the metering substrate is enabled~].~&"
	      (mi-enabled?))
      (format t "~&Metering is currently counting down from ~A, ~
                 ~:[but the metering substrate is disabled~
                  ~;and the metering substrate is enabled~].~&"
	      *iteration-metering-countdown*
	      (mi-enabled?)))
  (let ((current *clock-tick*))			; Avoid anomalous results.
    (when (delayed-trigger-valid?)
      (format t "~&There is a delayed trigger set for ~S, "
	      *iteration-metering-trigger*)
      (let ((tick (car *iteration-metering-trigger*)))
	(cond ((< tick current)
	       (format t "but that time is ~A clock tick~:P in the past."
		       (- current tick)))
	      ((> tick current)
	       (format t "which is ~A clock tick~:P in the future."
		       (- tick current)))
	      ((= tick current)
	       (format t "which starts right now."))
	      (t
	       (error "Well, this sure shouldn't have happened!"))))))
  (values))

#+Genera					; This one matters, because of PROCESS: package.
(defun TRACK-METER ()
  (unless (meter-zero?)
    (terpri)
    (loop until (meter-zero?)
	  do (let ((old *iteration-metering-countdown*))
	       (format t "~A " old)
	       (process:process-wait "Countdown wait"
				     #'(lambda ()
					 (not (= old *iteration-metering-countdown*))))))
    (format t "0~&"))				; Looks prettier to end with a zero...
  (values))

;;; Noticing the variables above and enabling metering appropriately.
(defun RUN-ONE-ITERATION (number-of-actions)
  ;; To allow synchronously starting and stopping the metering interface after
  ;; running a particular integer number of iterations.  To use this, use the command
  ;; :Meter In Process :Only When Enabled, and set *ITERATION-METERING-COUNTDOWN*
  ;; to some integer.
  #-Genera (run-one-iteration-1 number-of-actions)
  #+Genera (progn
	     (when (and (delayed-trigger-valid?)
			(= *clock-tick* (car *iteration-metering-trigger*)))
	       (setf *iteration-metering-countdown* (second *iteration-metering-trigger*)))
	     (cond ((and (numberp *iteration-metering-countdown*)
			 (not (zerop *iteration-metering-countdown*)))
		    (mi:with-metering-enabled
		      (run-one-iteration-1 number-of-actions))
		    (decf *iteration-metering-countdown*)
		    (maybe-beep-upon-completion))
		   (t
		    (run-one-iteration-1 number-of-actions)))))

;;;; Actually running an iteration.

;;; +++
;;; Various little switches for playing games with how an iteration runs.
;;; Totally ad-hoc, and used only while I'm testing parts of the logic.

;;; Allow turning off goal-directed-action stuff, since the current ('89) implementation takes so much time.
;;; I haven't made this a DEF-DEBUGGING-SWITCH, because this really isn't for debugging, and the extra
;;; macros that would define aren't really necessary.  I don't anticipate changing this until it's time to
;;; add goal-directed actions.
(defswitch *INHIBIT-UPDATE-ACCESSIBILITY* t update-accessibility)

;;; Use new spinoff-schema algorithms.
(defswitch *USE-NEW-MAYBE-SPINOFF-SCHEMA* t new-maybe-spinoff-schema)
;;; ---

;;; +++
;;; &&& The random DEFVARs below are actually implementing a bag of behavior:
;;; how to allow the running of a single iteration to call out to certain critical
;;; microworld housekeeping functions.  This should be done better, probably by
;;; having the DEF-MICROWORLD form, now that it exists, set these in an instance
;;; somewhere, and having something then check the instance.  Later.
(defvar *MICROWORLD-STATUS-REPORTING-FUNCTION* #'error)
(defvar *MICROWORLD-STATUS-REPORTING-FUNCTION-ARGS*
	'("SCHEMA::*MICROWORLD-STATUS-REPORTING-FUNCTION* wasn't initialized for this microworld."))

;;; Note that ARGS is _not_ a &REST arg!  [For convenience of DEF-MICROWORLD-TOPLEVEL's call.]
(defmacro WITH-MICROWORLD-STATUS-REPORTING-FUNCTION ((fn args) &body body)
  `(let ((*microworld-status-reporting-function* ,fn)
	 (*microworld-status-reporting-function-args* ,args))
     ,@body))

(defun MICROWORLD-STATUS-REPORT ()
  (if *microworld-status-reporting-function-args*	; If this is NIL, there were no args.  (If there was an arg of NIL, this would be (NIL).)
      (apply *microworld-status-reporting-function*
	     (car *microworld-status-reporting-function-args*)
	     (cdr *microworld-status-reporting-function-args*))
      (funcall *microworld-status-reporting-function*)))

(defvar *MICROWORLD-CLOCK-TICK-FUNCTION* nil)	; We'll explode if this isn't set, which is correct.

(defmacro WITH-MICROWORLD-CLOCK-TICK-FUNCTION (fn &body body)
  `(let ((*microworld-clock-tick-function* ,fn))
     ,@body))

(defun MICROWORLD-CLOCK-TICK ()
  (funcall *microworld-clock-tick-function*))

;;; &&& I _really_ shouldn't be duplicating the code from the status-reporting function!
;;; This should either all be macroized, or it should all be an instance or some other
;;; bag o' policy.  Geez...  But later.

;;; By default, we pick actions randomly, but allow rebinding this to some different generator.
;;; The selection function must return the index of some action to be executed.
;;;
;;; See also USUALLY-GOAL-DIRECTED-RANDOM-ACTION-SELECTOR, which assumes that
;;; #'RANDOM NIL are the defaults here.
(defvar *MICROWORLD-ACTION-SELECTION-FUNCTION* #'random)
(defvar *MICROWORLD-ACTION-SELECTION-FUNCTION-EXTRA-ARGS* nil)

;;; WITH-ACTION-SELECTION is now defined in GOALS.LISP, to avoid a forward reference
;;; from there to here, and to put it where it makes more sense.

(defun MICROWORLD-ACTION-SELECT (number-of-actions)
  (unless *microworld-action-selection-function*	; Shouldn't happen, unless WITH-MICROWORLD-ACTION-SELECT-FUNCTION was called with garbage.
    (error "~S wasn't initialized for this microworld."
	   '*microworld-action-selection-function*))
  (apply *microworld-action-selection-function*
	 number-of-actions 
	 *microworld-action-selection-function-extra-args*))
;;; ---

;;; A meter for debugging.
(def-vector-metering-counter *ACTIONS-THIS-RUN*)

;;; For performance evaluation in chaining to goals.  Turns off the learning system,
;;; but allows actions to update the microworld, keeps items' states updated, and
;;; allows the microworld to take random actions on its own (e.g., the random object
;;; movement stuff).
(defswitch *LEARNING-INHIBITED* nil learning-inhibited)

;;; The actual guts of running an iteration.  Note that half of the statements here are for debugging.
;;;
;;; NOTE:  Strictly speaking, the call to SCHEMA-UPDATE-APPLICABLE just _before_ we execute an action shouldn't be necessary.
;;; But all the 1-iteration runs in goal evaluation seem to be occasionally failing to update schemas' applicabilities, leading to planning
;;; failures because we don't have an accurate idea of what schemas are really applicable.  I doubt that it's because we're being
;;; thrown through from some signaller in PICK-BEST-NEXT-ACTION-TO-GOAL or whatever (both because those all signal _before_
;;; returning an action to execute, hence we couldn't have executed one), and on circumstantial evidence from instrumented runs.
;;; But hopefully this will sweep the problem under the rug well enough that I don't have to nail it precisely.  (*sigh*)  Since it
;;; only takes ~100ms to run, even with ~3K schemas, this seems a small price to pay for correctness (even if I don't understand it).
;;;
;;; NOTE also that any action that signals, throws, or otherwise transfers nonlocally had damned well better do it BEFORE executing!
;;; (In other words, throwing while computing what action to perform is fine, but throwing after performing it is not allowed.)  If this
;;; constraint is not obeyed, everything after that point in this routine would have to be placed in an UNWIND-PROTECT cleanup handler
;;; so that things get updated.  (I don't do that now because there's another bad problem that it would expose:  if such an action did
;;; such a thing, ACTION-INDEX would presumably be invalid.  But we store it for later analysis---what would we store?  I don't want
;;; to have to make everything either slip one iteration, or have to deal with non-numbers in the stored trace, or whatever, until I'm
;;; forced to do so.)
(defun RUN-ONE-ITERATION-1 (number-of-actions)
  (when *goal*					; If we're being goal-directed, see if it's time to switch goals.
    ;; &&& Maybe I should keep a global flag around that tells me whether I've initialized this, since this has to at least scan all goals, etc, each iteration.
    ;; &&& Or perhaps I should just initialize it whether or not *GOALS* it bound, and use *CLOCK-TICK* = 0 as my flag.  I dunno.
    (maybe-initialize-goal-sets)
    (setf *goal* (goal-fsm *goal*)))
  (schema-update-applicable)			; See comment at start.
  (let ((action-index (microworld-action-select number-of-actions)))
    (show-items-format "initial state ~%")	; Reporting.
    (microworld-status-report)			; Reporting.
    (schema-update-applicable)			; Work. [This is required for goals' use of ACCESSIBLE-TEST, even if UPDATE-ACCESSIBILITY isn't being called.]
    (unless *inhibit-update-accessibility*	; &&& ---> DBG!!!
      (update-accessibility))			; Work.  [Biggest hotspot.]
    (push action-index *actions-this-run*)	; Reporting.
    (funcall (get-action action-index))		; Work.  ---> Do some random action.
    (microworld-clock-tick)			; Work.  [&&& EYEHAND's version of this returns the new clock.  Should I make mudworld's do the same?]
    (incf *clock-tick*)				; This is useful even in the static navigation task as an iteration counter elsewhere.
    (item-update-state)				; Work.
    (conj-update-state)				; Work.
    (schema-update-activated action-index)	; Work.
    (syn-item-update-state)			; Work.
    (store-show-items)				; Reporting.
    (update-changed-state-item-history)		; Work.  [New.  Maybe I should put it inside ITEM-UPDATE-STATE; I dunno.  What about conj's?]
    (debug-format "~%~A~%"			; Reporting....
		  (schema-print-name (get-schema action-index)))	; ...
    (print-state-array				; Reporting.
      *microworld-state*			; ...
      *microworld-state-size*)			; ...
    (microworld-status-report)			; Reporting.
    (schema-update-all-results)			; Work.
    (schema-update-reliability)			; Work.
    (update-predicted-results)			; Work.
    (print-predicted-results)			; Reporting.
    (unless *learning-inhibited*		; This is the strictly learning-related stuff.
;       (schema-update-ext-item-stats)		; Work.  [Third biggest hotspot.]
      (new-schema-update-ext-item-stats)	; Work.
      (schema-update-ext-conj-stats)		; Work.
      (cond (*use-new-maybe-spinoff-schema*
; 	     (new-maybe-spinoff-schema)
; 	     (inside-out-maybe-spinoff-schema)
	     (reconfigurable-inside-out-maybe-spinoff-schema)	; Work.
	     )
	    (t					; &&& ---> DBG!!!
	     (maybe-spinoff-schema)))		; Work.  [Second biggest hotspot.]
      (maybe-make-conjs)			; Work.
      (maybe-make-syn-items)			; Work.
      )						; End of UNLESS.
    ))

(defun LMFS-FILESYSTEM? (pathname)
  #-Genera pathname nil				; PATHNAME ignored.
  #+Genera (typep (scl:send (scl:send (scl:send pathname :pathname) :truename) :host)
		  'fs:lispm-host))

(defmacro WITH-LOGGING-FILE ((pathname stream-variable) append? &body body)
  `(let* ((pathname (pathname pathname))
	  (real-if-exists?
	    (if ,append?
		(cond ((probe-file pathname)
		       :append)
		      (t
		       (format *error-output* "~&Warning:  ~A didn't already exist,~&~
                                                 but you asked to append to it.  Creating a new file instead...~&"
			       pathname)
		       :new-version))
		:new-version)))
     (with-open-file (,stream-variable ,pathname
		      :direction :output
		      ;; I wonder if there was some Lucid or MCL reason that Ramstad said :SUPERSEDE?
		      ;; In any event, punt completely for now, and stick in something like the code below
		      ;; if MCL does the wrong thing with this somehow.
		      :if-exists real-if-exists?
;                     :if-exists (if (lmfs-filesystem? ,pathname) :new-version :supersede)
		      )
       (unwind-protect
	   (progn ,@body)
	 ;; If the body got an error or otherwise threw out, finish output before allowing
	 ;; the logfile to be closed.  (If the body finished normally, then we'll presumably
	 ;; do a normal close from the WITH-OPEN-FILE above, and everything will be copacetic,
	 ;; but an error will cause us to throw past it, resulting in an abnormal close.)
	 (main-format "~A[End of run.]~&"
		      (emit-date-prefix nil nil))
	 (finish-output ,stream-variable)))))

;;; If COND is NIL, just run the body the stream-variable bound to the null stream.
(defmacro WITH-LOGGING-FILE-IF (cond (pathname stream-variable) append? &body body)
  `(cond (,cond
	  (with-logging-file (,pathname ,stream-variable) ,append?
	    ,@body))
	 (t
	  (let ((,stream-variable
		 #+Genera #'si:null-stream
		 #-Genera (unimplemented)))	; I have no idea how to create a null stream portably.
	    ,@body))))
  
(defparameter *DEFAULT-LOG-DIRECTORY*
	      #+Genera "SCHEMA: RUNS;"
	      #-Genera "")			; %%% Use default directory for now.

(defun LOG-PATHNAME (dir count &optional (name-prefix ""))
  ;; If COUNT is NIL, we'll assume that we're being run as many minor iterations
  ;; in only one major iterations, hence appending the "-0" that we used to always
  ;; do is silly, and we won't.  If COUNT is non-NIL, we'll assume that it should be
  ;; added to the pathname, with a prepended dash.  (Note that, for future flexibility,
  ;; we actually print could with ~A, not ~D, so it could even be a string...)
  (merge-pathnames (format nil "~A~:[~;-~:*~A~].out" name-prefix count)
		   dir))

;;; Desired width of the timestamp.  We do it in this way so that other things
;;; may be aligned with it without magic numbers.  Note that we guarantee
;;; (with a width argument to ~A) that it will be this wide when printed, even
;;; though the datestamp can change width by a character (under Genera)
;;; depending on whether the month is less than 10 or not.
(defparameter *DEFAULT-DATE-PREFIX-LENGTH* #-Genera 0 #+Genera 19)

(defun EMIT-DATE-PREFIX (&optional (newline t) (stream t))
  (when newline
    (terpri stream))
  #-Genera (format stream "")
  #+Genera (format stream "~VA"
		   *default-date-prefix-length*
		   (format nil "~\\datime\\")))

(defparameter *HOW-OFTEN-TO-FINISH-OUTPUT* 10)	; Seconds.
(defvar *LAST-UT-OUTPUT-FINISHED* 0)

(defun MAYBE-FINISH-OUTPUT (stream)
  ;; If this returns non-NIL, it had to do a finish-output, for whatever that's worth...
  (when (> (get-universal-time)
	   (+ *last-ut-output-finished* *how-often-to-finish-output*))
    (finish-output stream)
    (setf *last-ut-output-finished* (get-universal-time))))	; Called twice, since time may have elapsed while we were finishing.

(defparameter *SHOW-ITERATIONS* t)
(defparameter *NOTE-LOG-APPENDS* t)
(defparameter *DONT-SHOW-RELIABLE-SCHEMAS-DURING-SUMMARIES* t)

(defvar *SYNCHRONOUS-STOP* nil)			; Set this non-NIL to stop after the current iteration.

;;; So I can be guaranteed that I typed it in the right package.
;;; (E.g., (SYNC-STOP) will err or work, whereas (SETF *SYNCHRONOUS-STOP* T)
;;; can fail silently if I'm in the wrong package.)
(defun SYNC-STOP ()
  (setf *synchronous-stop* t))

(defun RESET-SYNC-STOP ()
  (when *synchronous-stop*
    (format t "~&Warning:  ~S was ~S.  Resetting it...~&"
	    '*synchronous-stop* *synchronous-stop*)
    (setf *synchronous-stop* nil)))

;;; +++ Things reported at the start of a run.
(defun SUMMARIZE-GENERATORS (&optional (stream t) (indent *default-date-prefix-length*))
  (let ((max-label-length
	  (longest-printed-rep *iterator-generators-in-use* #'car)))
    (loop for (label schema-symbol item-symbol) in (reverse *iterator-generators-in-use*)	; Reversed for aesthetics of definition order.
	  do (safe-format stream "~&~V@T~@(~A~)~V@T item   generator is ~A~&~
                                  ~4:*~V@T~@(~A~)~V@T~* schema generator is ~A~2&"
			  indent label (- max-label-length (length label))
			  (symbol-value item-symbol)
			  (symbol-value schema-symbol))))
  (values))

(defun SUMMARIZE-REPORTED-PARAMETERS (&optional (stream t) (indent *default-date-prefix-length*))
  (let ((max-symbol-length
	  (longest-printed-rep *reported-parameters*
			       #'(lambda (entry)
				   (symbol-name (car entry))))))
    (loop for (symbol init-form) in (reverse *reported-parameters*)     ; Reversed for aesthetics of definition order.
	  do (safe-format stream "~&~V@T~A~V@T = ~S~:[ (but its init-form~:[, ~S,~;~*~] would set it to ~S)~;~]~&"
			  indent symbol (- max-symbol-length (length (symbol-name symbol)))
			  (symbol-value symbol)
			  (equalp (symbol-value symbol) (eval init-form))
			  (equalp init-form (eval init-form))
			  init-form
			  (eval init-form))))
  (when *reported-parameters*
    (format stream "~2&"))
  (values))

(defun SUMMARIZE-ACTION-SELECTOR (&optional (stream t) (indent *default-date-prefix-length*))
  (let* ((report-these
	   (remove nil '(*microworld-action-selection-function*
			  *microworld-action-selection-function-extra-args*)
		   :key #'symbol-value))
	 (max-symbol-length
	   (longest-printed-rep report-these #'symbol-name)))
    (loop for symbol in report-these
	  do (safe-format stream "~&~V@T~A~V@T = ~:[~S~;~A~]~&"
			  indent symbol (- max-symbol-length (length (symbol-name symbol)))
			  (functionp (symbol-value symbol))	; Print functions as their names, not at #<Compiled function FOO>.
			  (symbol-value symbol)))
    (when report-these
      (format stream "~2&")))
  (values))

(def-debugging-switch REPORT-STARTING-GOAL t)	; Only report the very first goal.

(defun SUMMARIZE-FIRST-GOAL (&optional (stream t) (indent *default-date-prefix-length*))
  (let ((*output-stream* stream))
    (if *goal*
	(report-starting-goal-format
	  "~&~V@TInitial goal = ~A~2&"
	  indent
	  (goal-name *goal*))	; GOAL is a structure, not a name.
	(report-starting-goal-format
	  "~&~V@TNo initial goal.~2&"
	  indent))))
;;; ---

;;; +++ Things reported every "major loop" clump (default 50 iterations).
(defun SUMMARIZE-NONSCHEMA-SCALARS (&optional (stream t) (indent *default-date-prefix-length*))
  ;; E.g., those scalars associated with the schema DB that aren't the schema counters themselves.
  ;; NOT scalars associated with some microworld.  This needs a better name, I guess.
  ;; We therefore don't report on *SCHEMA-NUMBER* or *CLOCK-TICK*.  We also don't
  ;; report *ACTION-NUMBER* (which should stay constant), and probably shouldn't
  ;; bother reporting (- *ITEM-NUMBER* *SYN-ITEM-NUMBER*) (which I believe to be
  ;; the number of primitive items, another constant), but we will until I'm sure of that.
  (safe-format stream "~&~V@T~D conjunction~:P, ~D item~:P (~D primitive, ~D synthetic).~&"
	       indent
	       *conj-number*
	       *item-number*
	       (primitive-item-number)	; This shouldn't ever change, right?  Let's check that assumption...
	       *syn-item-number*)
  (values))

(defun SUMMARIZE-PERIODICALLY-REPORTED-SCALAR-METERS (&optional (stream t) (indent *default-date-prefix-length*))
  (loop for (symbol label) in (reverse *scalar-metering-counters-periodically-reported*)	; Reversed for aesthetics of definition order.
	do (safe-format stream "~&~V@T~A~&"
			indent
			(format nil label
				(symbol-value symbol))))
  (values))

(defun SUMMARIZE-PERIODICALLY-REPORTED-VECTOR-METERS (&optional (stream t) (indent *default-date-prefix-length*))
  (loop for (symbol label) in (reverse *vector-metering-counters-periodically-reported*)	; Reversed for aesthetics of definition order.
	do (ignore symbol)
	do (safe-format stream "~&~V@T~A~&"
			indent
			(format nil label
				(reduce #'+ (symbol-value symbol)))))
  (values))
;;; ---

;;; If REAL-TIME-NOT-TO-EXCEED is supplied, we'll run until we've used up that many seconds,
;;; even if we haven't done the indicated number of iterations.  This is to allow checkpointing
;;; at regular (real-time) intervals, in place of or in addition to checkpointing at regular (iteration)
;;; intervals.
;;;
;;; If ARBITRARY-STOP-FN is supplied, we'll call it before each iteration.  If it returns
;;; non-NIL, we'll stop.  If what it returns is a string, we'll print that as the stop reason.
(defun RUN-MICROWORLD (number-of-actions iterations-per-major &key
		       (major-iterations 1)
		       (report-reliable-schemas-every-n-iterations
			 (min iterations-per-major 50))
		       real-time-not-to-exceed
		       start-new-run
		       arbitrary-stop-fn
		       (log-enabled t)
		       (log-append (not start-new-run))
		       (log-name-prefix "")
		       (log-directory *default-log-directory*))
  (assert (or (null real-time-not-to-exceed)
	      (and (numberp real-time-not-to-exceed)
		   (> real-time-not-to-exceed 0))))
  (dotimes (major major-iterations)
    (let ((pathname
	    (log-pathname log-directory
			  (unless (= major-iterations 1)
			    major)
			  log-name-prefix))
	  (start
	    (if start-new-run
		0
		*clock-tick*))
	  (end
	    (if start-new-run
		iterations-per-major
		(+ iterations-per-major *clock-tick*))))
      (with-logging-file-if log-enabled (pathname *output-stream*) log-append
	(when (and (not (start-of-run?))
		   log-append
		   *note-log-appends*)
	  (main-format "~A[Continuing run...]~&"
		       (emit-date-prefix nil nil)))
	(summarize-generators          *output-stream* 0)
	(summarize-reported-parameters *output-stream* 0)
	(summarize-action-selector     *output-stream* 0)
	(summarize-first-goal          *output-stream* 0)
	(flet ((report-statistics (iteration)
		 (show-reliable-schemas iteration *dont-show-reliable-schemas-during-summaries*)
		 (summarize-nonschema-scalars *output-stream*)
		 (summarize-periodically-reported-scalar-meters *output-stream*)
		 (summarize-periodically-reported-vector-meters *output-stream*)
		 (main-format "~%")		; Add a blank line in between them...
		 (maybe-finish-output *output-stream*)))	; Flush output to file so I can monitor execution in progress.
	  (loop with starting-ut = (when real-time-not-to-exceed
				     (get-universal-time))
		for minor from start below end
		for iteration = (+ minor (* major iterations-per-major))
		;; If we enter the loop with this set, something's probably wrong---
		;; stop immediately, rather than running a single iteration and then stopping.
		;; (If the user had wanted to run just one iteration [single step?], he'd have _said_ so!)
		;; Note that entering this way won't note the sync-stop in the log; too bad.
		until *synchronous-stop*
		until (and starting-ut
			   (> (- (get-universal-time) starting-ut)
			      real-time-not-to-exceed))
		for stop-reason = (and arbitrary-stop-fn
				       (funcall arbitrary-stop-fn))
		when stop-reason
		  do (main-format "~A~:[Arbitrary-stop function requested a stop.~;~A~]~&"
				  (emit-date-prefix nil nil)
				  (stringp stop-reason)
				  stop-reason)
		     (report-statistics iteration)
		until stop-reason
		do
	    (assert (= iteration *clock-tick*))	; ---> Let's leave this, and the math above, in until I'm surer about when my clock should tick.
	    (when *show-iterations*		; [... the ASSERT call takes only 11 uS more than simple =, and only 18 uS total:  it's in the noise.]
	      (when (zerop (mod minor 20))	; 20 iteration numbers per line (suitable for 4-digit numbers, but not 5, on a Genera main screen).
		(emit-date-prefix))		; [Suitable for 5 if there aren't tombstone borders.]
	      (format t "~D " iteration))
	    (run-one-iteration number-of-actions)	; Actually do the work!
	    (when (or *synchronous-stop*
		      (and (zerop (mod iteration report-reliable-schemas-every-n-iterations))
			   (not (zerop iteration)))
		      (= iteration (1- end)))
	      (when (or *synchronous-stop* (= iteration (1- end)))
		(main-format "~A[~:[End of run~;Synchronous stop requested~].  Final data follows.]~&"
			     (emit-date-prefix nil nil)
			     *synchronous-stop*))
	      (report-statistics iteration)
	      ))))
      (unless *learning-inhibited*		; If learning is inhibited, this can't have changed.
	(format t "~&Number of schemas: ~D~&" *schema-number*))))
  (values))

(defvar *PRESERVED-RANDOM-STATE* nil)
(defparameter *PRESERVED-RANDOM-STATE-PATHNAME*
	      #+Genera "schema:code;preserved-random-state.ibin"
	      #+LispWorks "preserved-random-state.mfasl-special"	; So i can zap all MFASLs undiscriminatingly.
	      #-(or Genera LispWorks) "preserved-random-state")

;;; No actual callers of this guy at the moment. --- Foner 16 Jul 93.
(defun NEW-RANDOM-EPOCH ()
  (setf *preserved-random-state* (make-random-state t))
  (setf *random-state* (make-random-state *preserved-random-state*)))

;;; Call this once to build the file, from a FRESHLY BOOTED machine (since all my previous
;;; runs were from the same freshly booted machine, and I haven't switched releases yet).
;;; Note that this not only saves me from a prior call to RANDOM before the first run in the
;;; session, but from changes to the seed if I switch releases.
(defun SAVE-VIRGIN-RANDOM-STATE (&optional (where *preserved-random-state-pathname*))
  (setf *preserved-random-state* (make-random-state))	; Same as (MAYBE-INITIALIZE-RANDOM T NIL), but clearer...
  (dump-variable-sharing where *preserved-random-state*))

(defun MAYBE-INITIALIZE-RANDOM (init? &optional
				(read-from-file t)
				(where *preserved-random-state-pathname*))
  ;; If INIT? is NIL, we don't do anything.  If it's non-NIL:
  ;;  If *PRESERVED-RANDOM-STATE* is set, we set *RANDOM-STATE* to a copy of it.
  ;;   Otherwise, we check READ-FROM-FILE.  If that's non-NIL, then we read a random-state
  ;;   from the file in WHERE, setting *PRESERVED-RANDOM-STATE* as we read the file.  If
  ;;   it was NIL, we set *PRESERVED-RANDOM-STATE* to a copy of the current *RANDOM-STATE*.
  ;; The idea here is to allow us to call this function with INIT? non-NIL and get the random
  ;; number generator reset to the last time we set *PRESERVED-RANDOM-STATE*.  However,
  ;; the first time we call it, that variable isn't set, so we'll set it, such that on the second and
  ;; subsequent calls, we'll reset the random number generator to its state on the very first call.
  ;; If you want to actually change that "fixed point", you need to call NEW-RANDOM-EPOCH.
  ;; The READ-FROM-FILE business lets us correctly reinitialize the random number generator
  ;; if we start a run and we have NEVER run before in this boot session, but somebody else
  ;; has had occasion to call RANDOM.
  (when init?
    (cond (*preserved-random-state*
	   (setf *random-state* (make-random-state *preserved-random-state*)))
	  (t
	   (cond (read-from-file
		  (let ((*standard-output* *error-output*))	; Make the load message appear in my window, not in the log.
		    (load where)))		; Emit a message if we do this, for reassurance and so I know it's really happened.
		 (t
		  (setf *preserved-random-state* (make-random-state))))))))

#+Genera
(defun PRINT-RANDOM-STATE (state &optional (stream t))
  ;; Until I'm running 8.3 or some world known to dump random-states correctly...
  ;; Note that this is probably Just Not Right no matter what.
  ;; (Now superceded by real pre-8.3 random-state dumping.)
  (format stream "~&~S~&~S~&"
	  state
	  (zl:listarray (fcli::random-array state)))
  (values))

(defun START-OF-RUN? ()
  ;; In case I change my mind about what it means to be at the start of a run,
  ;; let's make this easy to find & change...
  (zerop *clock-tick*))  

(def-microworld-independent-init BASH-ALL-ARRAYS-TO-NIL ()
  ;; Individually bash all the various arrays we use back to NIL before reinitializing
  ;; them.  This makes no difference to normal operation (since we never access
  ;; anything beyond the relevant *...-NUMBER* variable), but a _big_ difference to
  ;; snapshotting, since the snapshotting code dumps _every_ array element, not
  ;; just the active ones.  This means that arrays that grow non-NIL elements during
  ;; a run will "shrink" back down and save quickly if I start a run over.  This bashes
  ;; the arrays all the way out to the maxima, _not_ just to their current limit, since
  ;; if we've _ever_ managed to hit a _higher_ limit (call it the high-water-mark)
  ;; without running this, we'll leave a _gap_ and won't clear the rest!  Damn!  I spent
  ;; about two hours debugging _that_ particular screwup!
  (dotimes (i *conj-maximum*)
    (setf (aref *conj-array* i) nil))
  (dotimes (i *item-maximum*)
    (setf (aref *item-array* i) nil))
  (dotimes (i *syn-item-maximum*)
    (setf (aref *syn-item-array* i) nil))
  (dotimes (i *schema-maximum*)
    (setf (aref *schema-array* i) nil))
  (values))

;;; CLEAR-ALL-METERS is specialized to clear all meters:  those quantities that get
;;; snapshotted and saved between runs.  These functions are a catchall for all the
;;; other microworld-independent quantities that must be reset at the beginning of a
;;; run.  They're here both to package all of them up neatly, and to avoid having to
;;; constantly change DEF-MICROWORLD-TOPLEVEL and thus having to recompile all of
;;; its callers.
;;;
;;; This is what actually runs things created by DEF-MICROWORLD-INDEPENDENT-INIT.
(defun CLEAR-ALL-NONMETERS-PRE-MICROWORLD-INIT ()
  (loop for fn in (reverse *microworld-independent-init-functions*)
	  do (funcall fn))
  (values))

;;; Runs all initializers for this world that must be run after the world is set up.
;;; We run the list in reverse order so that they run in the order in which they
;;; were defined by DEF-POST-MICROWORLD-INIT; see that macro for details.
(defun CLEAR-ALL-NONMETERS-POST-MICROWORLD-INIT ()
  (loop for fn in (reverse *current-microworld-post-microworld-init-functions*)
	when (eq (symbol-package fn)
		 *current-microworld-package*)
	  do (funcall fn))
  (values))

;;; We don't warn if we're being asked to start a new run, since this is not the
;;; default and hence the user had to go to extra effort to start one.  However, if it
;;; looks like we haven't run one before, we'll initialize things without warning.  This
;;; means that any given microworld had better interact correctly with
;;; START-OF-RUN? if it expects to be safe!
;;;
;;; Note that we expect that any microworld built with DEF-MICROWORLD and the
;;; macros it generates will set *NUMBER-OF-ACTIONS* in the appropriate package for
;;; the microworld.  We still ask for it here, since it avoids having to remodularize all
;;; the code called below here that expects this value to be passed down.  (Thus, in a
;;; perfect world, we shouldn't bother to ask.)  We _could_ use it to divine the
;;; package of interest, but I don't see any code right now that actually does this
;;; [&&& As of Schema 11.3, I fixed a comment that claimed that it was used to divine
;;; packages to instead be the above.]
;;;
;;; We use the package of INIT-FUNCTION as the canonical package for the microworld
;;; (used by WITH-CURRENT-MICROWORLD and things dependent upon it).  We don't use
;;; NUMBER-OF-ACTIONS (for example), because there's no guarantee that it'll actually
;;; be *NUMBER-OF-ACTIONS* (though it's likely to be), while the INIT-FUNCTION is
;;; pretty likely to actually be in the right package.  Because SYS:FUNCTION-NAME is
;;; Genera-specific, this means that the function has to be named by a symbol, e.g.,
;;; 'FUNCTION, not #'FUNCTION, since otherwise we can't get its package!  *sigh*
;;; I don't really have to worry about the trivial slowdown of indirection through
;;; the symbol, since it's only called once in a whole run (even if it was called every
;;; iteration, of course, the few hundred nanoseconds or whatever for the lookup is
;;; totally lost in the noise!).
;;;
;;; We don't use NAME because that's usually in the SCHEMA: package, to make it easier
;;; for me to call (what I _should_ do is to just export NAME, so it's easy to call from
;;; anywhere, though I'd have to make SCHEMA: :USE all of its microworld's package,
;;; which seems excessive).
;;;
;;; For things like the Hamsterdam linkage, which requires setting up and tearing
;;; down network connections and the like, there are two hooks for getting things
;;; executed at the end of a run, as follows:
;;;
;;; RUN-CLEANUP-FN is executed as an UNWIND-PROTECT cleanup handler after
;;; execution terminates.
;;; 
;;; RUN-WRAPPER-FORM allows you to wrap some particular form around execution.
;;; Notice where it is wrapped:  _inside_ the various forms that bind the current
;;; microworld, etc, in case their state is needed (this presupposes that the wrapper
;;; isn't necessary for _them_; I had to choose one way or the other or I'd have to
;;; supply two hooks!).  Note that it is also wrapped _inside_ the UNWIND-PROTECT
;;; used for RUN-CLEANUP-FN, in case for some reason you've supplied both.  Note
;;; also that you should probably make the RUN-WRAPPER-FORM be an UNWIND-PROTECT
;;; of some sort, in case execution aborts.  The wrapper must take a single body arg.
(defmacro-exported-definer DEF-MICROWORLD-TOPLEVEL (name number-of-actions init-function
						    microworld-status-reporter microworld-clock-tick-fn
						    &key
						    init-function-args
						    microworld-status-reporter-args
						    start-new-run-default
						    arbitrary-stop-fn
						    (recapitulate-randomness-default 'start-new-run)	; Yes, quoted, and no, not -DEFAULT.
						    (log-append-default '(not start-new-run))	; Ditto.
						    (major-iterations-default 1)
						    real-time-not-to-exceed-default	; E.g., forever.
						    (report-reliable-schemas-every-n-iterations-default '(min iterations-per-major 50))
						    (log-enabled-default t); If NIL, *OUTPUT-STREAM* is bound to the null stream, and we won't log.
						    (log-name-prefix-default "")
						    (log-directory-default '*default-log-directory*)
						    (run-wrapper-form 'progn)	; Optional wrapper around execution.
						    (run-cleanup-fn #'ignore))	; Executed at the end as an UNWIND-PROTECT cleanup handler.
  #+Genera (declare (zwei:indentation 1 1))
  `(defun ,name (iterations-per-major &key
		 (start-new-run (or ,start-new-run-default (start-of-run?)))
		 (arbitrary-stop-fn ,arbitrary-stop-fn)
		 (recapitulate-randomness ,recapitulate-randomness-default)
		 (log-append ,log-append-default)
		 (major-iterations ,major-iterations-default)
		 (real-time-not-to-exceed ,real-time-not-to-exceed-default)
		 (report-reliable-schemas-every-n-iterations ,report-reliable-schemas-every-n-iterations-default)
		 (log-enabled ,log-enabled-default)
		 (log-name-prefix ,log-name-prefix-default)
		 (log-directory ,log-directory-default)
		 (run-cleanup-fn ,run-cleanup-fn))
     (with-current-microworld ,init-function
       (with-microworld-status-reporting-function (,microworld-status-reporter ,microworld-status-reporter-args)
	 (with-microworld-clock-tick-function ,microworld-clock-tick-fn
	   (unwind-protect
	       (,run-wrapper-form
		 (reset-sync-stop)
		 (when start-new-run
		   (clear-all-nonmeters-pre-microworld-init)
		   (apply ,init-function ,init-function-args)
		   (clear-all-nonmeters-post-microworld-init)
		   ;; $OPT:  Right here would be a good place to compute ALL-BARE-SCHEMAS if I wanted to be more efficient on each iteration...
		   (clear-all-meters))
		 (maybe-initialize-random recapitulate-randomness)
		 (run-microworld
		   ,number-of-actions iterations-per-major
		   :major-iterations major-iterations
		   :real-time-not-to-exceed real-time-not-to-exceed
		   :report-reliable-schemas-every-n-iterations report-reliable-schemas-every-n-iterations
		   :start-new-run start-new-run
		   :arbitrary-stop-fn arbitrary-stop-fn
		   :log-enabled log-enabled
		   :log-append log-append
		   :log-name-prefix log-name-prefix
		   :log-directory log-directory)
		 (funcall run-cleanup-fn))))))))

;;;; Saving and restoring the schema system state (microworld must also save and restore its state).

;;;; The actual workhorse routines.

(defparameter *SCHEMA-STATE-SCALARS*
	      '(
		*clock-tick*
		*schema-number*
		*item-number*
		*conj-number*
		*syn-item-number*
		*action-number*))

(defparameter *SCHEMA-STATE-SEQUENCES*
	      '(
		*microworld-state*
		*schema-array*
		*item-array*
		*conj-array*
		*syn-item-array*
		*action-array*
		*action-print-name-array*
		*accessible-item-pos*
		*accessible-item-neg*
		*accessible-conj-pos*
		*reliable-item-pos*
		*reliable-item-neg*
		*reliable-conj*
		*predicted-results*
		*predicted-result-conjs*))

(defparameter *SCHEMA-STATE-SEQUENCE-COPIERS*
	      ;; How to copy a single element of one of these sequences.
	      ;; If this stuff were Flavors-based or CLOS-based, of course,
	      ;; one could define copier functions (if they weren't already
	      ;; defined) and use type dispatch, etc.  What a pain...
	      ;; (FORTRAN in LISP!)  (Note also that we're mentioning the
	      ;; functions here using ' and not #', but oh well...)
	      '(
		(*microworld-state*        identity)
		(*schema-array*            safe-copy-schema)
		(*item-array*              safe-copy-item)
		(*conj-array*              safe-copy-conj)
		(*syn-item-array*          safe-copy-syn-item)
		(*action-array*            identity)
		(*action-print-name-array* identity)
		(*accessible-item-pos*     identity)
		(*accessible-item-neg*     identity)
		(*accessible-conj-pos*     identity)
		(*reliable-item-pos*       identity)
		(*reliable-item-neg*       identity)
		(*reliable-conj*           identity)
		(*predicted-results*       identity)
		(*predicted-result-conjs*  identity)))

(defparameter *SCHEMA-STATE-VARIABLES*
	      (append *schema-state-scalars*
		      *schema-state-sequences*))

;;; Leaving this set might make it impossible to restore certain old snapshots.  It
;;; should really get renamed, along with the spare slots, to account for their use
;;; for chaining (e.g., they're no longer spares, really, though they _can_ be
;;; regenerated, unlike the rest of a schema, so we're allowed to fail to reload them
;;; if necessary.)
(def-debugging-switch COPY-SCHEMA-CHAINING t)

(defun SAFE-COPY-SCHEMA (s)
  (let ((new (copy-schema s)))
    (setf (schema-context-array new)            (copy-seq (schema-context-array new))
	  (schema-context-children new)         (copy-seq (schema-context-children new))
	  (schema-result-children new)          (copy-seq (schema-result-children new))
	  (schema-result-conj-children new)     (copy-seq (schema-result-conj-children new))
	  (schema-extended-context new)         (copy-seq (schema-extended-context new))
	  (schema-extended-context-post new)    (copy-seq (schema-extended-context-post new))
	  (schema-extended-result-pos new)      (copy-seq (schema-extended-result-pos new))
	  (schema-extended-result-neg new)      (copy-seq (schema-extended-result-neg new))
	  (schema-extended-result-conj-pos new) (copy-seq (schema-extended-result-conj-pos new))
	  )
    ;; Currently knows about SPARE-SLOT-2's use for chaining!
    (when *copy-schema-chaining-enabled*
      (setf (schema-spare-slot-2 new)           (copy-seq (schema-spare-slot-2 new))))	; This is SCHEMA-CHAIN, a sequence.
    new))

(defun SAFE-COPY-ITEM (i)
  (let ((new (copy-item i)))
    (setf (item-context-dependent-schemas new)  (copy-seq (item-context-dependent-schemas new))
	  (item-result-dependent-schemas new)   (copy-seq (item-result-dependent-schemas new)))
    new))

(defun SAFE-COPY-CONJ (c)
  (let ((new (copy-conj c)))
    (setf (conj-item-array new)      (copy-seq (conj-item-array new))
	  (conj-pos-flag-array new)  (copy-seq (conj-pos-flag-array new))
	  (conj-neg-flag-array new)  (copy-seq (conj-neg-flag-array new))
	  (conj-inclusion-array new) (copy-seq (conj-inclusion-array new)))
    new))

(defun SAFE-COPY-SYN-ITEM (s)
  (let ((new (copy-syn-item s)))
    (setf (syn-item-on-duration new)  (copy-average (syn-item-on-duration new))
	  (syn-item-off-duration new) (copy-average (syn-item-off-duration new)))
    new))

(defun SCHEMA-COPIER-FROM-SYMBOL (symbol)
  (second (assoc symbol *schema-state-sequence-copiers*)))

(defun SAFE-COPY-SCHEMA-STATE-SEQUENCE (sequence-symbol old)
  (let ((copier (schema-copier-from-symbol sequence-symbol)))
    (if (eq (symbol-function copier) #'identity)
	(copy-seq old)				; Do it the fast way.
	(let ((new (make-array (length old)	; Do is the slow way.
			       :element-type (array-element-type old)))	; Only works for non-leader arrays, etc.
	      (old old))			; For Genera declaration below (stupid compiler).
	  #+Genera (declare (sys:array-register old new))
	  (dotimes (i (length old))
	    (when (aref old i)
	      (setf (aref new i)
		    (funcall copier (aref old i)))))
	  new))))

(defun SAFE-COPY-SCHEMA-STATE ()
  (append
    (loop for scalar-symbol in *schema-state-scalars*
	  collect (symbol-value scalar-symbol))
    (loop for sequence-symbol in *schema-state-sequences*
 	  collect (safe-copy-schema-state-sequence
		    sequence-symbol
		    (symbol-value sequence-symbol)))))

;;; Restoring our state from the above.

(defun SAFE-RESTORE-SCHEMA-STATE (snapshot)
  (loop for state-symbol in *schema-state-variables*
	for item in snapshot
	for item-is-sequence? = (member state-symbol *schema-state-sequences*)
	for restore-fn = (and item-is-sequence?
			      (schema-copier-from-symbol state-symbol))
	do (set state-symbol
		(if item-is-sequence?
		    (if (eq restore-fn #'identity)
			(copy-seq item)
			(safe-copy-schema-state-sequence state-symbol item))
		    item)))
  (values))

;;;; Some debugging functions for deriving what's in state variables,
;;;; either in the schema database or some microworld.

#+Genera					; ZL:TYPEP.
(defun STATE-VARIABLE-TYPES (&optional (state-variables *schema-state-variables*))
  ;; Debugging only.
  (mapc #'(lambda (symbol)
	    (format t "~&~A ~A~&"
		    (zl:typep (symbol-value symbol))
		    symbol))
	state-variables)
  (values))

(defun SEQ-DESCRIBE-FIRST-ELTS (&optional (which *schema-state-sequences*))
  (mapc #'(lambda (symbol)
	    (bold-format t "~2&~A:~&" symbol)
	    (describe (aref (symbol-value symbol) 0)))
	which)
  (values))

;;; This guy shouldn't be used, because it only copies toplevel structure.
(defun UNSAFE-COPY-SCHEMA-STATE ()
  ;; Umm...  this won't actually copy the state, of course, because individual
  ;; schemas in the schema array might get modified by something else, etc.
  ;; This only copies the toplevel structure.  Hmm.
  (append
    (loop for scalar-symbol in *schema-state-scalars*
	  collect (symbol-value scalar-symbol))
    (loop for sequence-symbol in *schema-state-sequences*
	  collect (copy-seq (symbol-value sequence-symbol)))))

;;;; General routines that grab both the schema state and a specific microworld's state.

(defvar *SNAPSHOTTED-WORLD-STATES* nil)

(defsubst NUMBER-OF-SNAPSHOTTED-WORLD-STATES ()
  (length *snapshotted-world-states*))

(defun MOST-RECENT-SNAPSHOT ()
  (car (last *snapshotted-world-states*)))

(defun SNAPSHOT-FROM-SPEC (snapshot)
  (cond ((consp snapshot)			; The particular snapshot represented by the list in SNAPSHOT.
	 snapshot)
	((numberp snapshot)			; The numbered snapshot represented by the number in SNAPSHOT.
	 (nth snapshot *snapshotted-world-states*))	; If invalid, this will explode.
	(t					; The most recent snapshot.
	 (most-recent-snapshot))))

;;; +++ Handy primitives.  These cannot be defined in SCHEMA-DEFS, because at least one of them
;;; +++ uses things like SCHEMA-CONTEXT-EMPTY-P, which is a macro that's defined in SCHEMA,
;;; +++ _after_ SCHEMA-DEFS.  So we'll put them here.

(defun SCHEMA-STATE (snapshot-spec)
  (let ((snapshot (snapshot-from-spec snapshot-spec)))
    (with-snapshot-destructuring snapshot
      schema-system-version timestamp clock-tick comment metering-state random-state microworld-state	; Ignored.
      schema-state)))

;;; Returns the entire schema vector.  This will includes trailing NILs!
(defun SCHEMA-STATE->SCHEMAS-AS-VECTOR (ss)	; Kluge!
  (nth 7 ss))

(defun PUSHED-VECTOR->LIST (vector)
  ;; A "pushed vector" is one built in the manner of VECTOR-PUSH-EXTEND.
  ;; The assumption here is that, once we see the first NIL, all the remaining
  ;; elements are going to be NIL.  This is the way the schema mechanism
  ;; builds vectors of schemas, conj's, and so forth.
  (let ((limit (position nil vector)))
    (listarray vector limit)))

;;; Returns only the schemas, as a list, not a vector.  Removes all NILs.
(defun SCHEMA-STATE->SCHEMAS-AS-LIST (ss)	; Kluge!
  (pushed-vector->list
    (schema-state->schemas-as-vector ss)))

;;; I seem to need this all the time...
(defun SNAPSHOT-SPEC->SCHEMAS-AS-LIST (snapshot-spec)
  (schema-state->schemas-as-list (schema-state snapshot-spec)))

(defun SCHEMA-STATE->TALLIED-SCHEMA-TYPES (schema-state)
  #+Genera (declare (values total context result result-conj bare))
  (let ((schemas (schema-state->schemas-as-list schema-state))
	(bare 0)
	(context 0)
	(result 0)
	(result-conj 0))
    (loop for schema in schemas
	  for c =  (not (schema-context-empty-p schema))
	  for r =  (not (schema-result-empty-p  schema))
	  for rc =      (schema-result-conj-p   schema)
	  do (cond ((not (or c r))
		    (incf bare))
		   (rc
		    (incf result-conj))
		   (c
		    (incf context))
		   (r
		    (incf result))))
    (values (length schemas)
	    context
	    result
	    result-conj
	    bare)))

;;; Should be using methods & argument type dispatching...  *sigh*...
(defun SNAPSHOT-SPEC->TALLIED-SCHEMA-TYPES (snapshot-spec)
  #+Genera (declare (values total context result result-conj bare))
  (schema-state->tallied-schema-types (schema-state snapshot-spec)))

;;; Returns the entire conj vector.  This will includes trailing NILs!
(defun SCHEMA-STATE->CONJS-AS-VECTOR (ss)	; Kluge!
  (nth 9 ss))

;;; Returns only the conj's, as a list, not a vector.  Removes all NILs.
(defun SCHEMA-STATE->CONJS-AS-LIST (ss)		; Kluge!
  (pushed-vector->list
    (schema-state->conjs-as-vector ss)))

;;; +++ Actual snapshotting code, and reporting functions to tell us what's been snapshotted.

;;; This MUST BE KEPT IN SYNC with WITH-SNAPSHOT-DESTRUCTURING and WITH-COMPLETE-SNAPSHOT-DESTRUCTURING!
;;; [Those two macros are defined in a much earlier file than this, because they're needed early in compilation.]
(defun-exported SNAPSHOT-WORLD-STATE (microworld-snapshotter-function &optional (comment ""))
  (setf *snapshotted-world-states*
	(append *snapshotted-world-states*
		(list (list (list #+Genera (multiple-value-list (sct:get-system-version 'schema))
				  #-Genera nil	; If not Genera, stuff a NIL in here.
				  #+Genera (format nil "~\\datime\\")
				  #-Genera nil	; Ditto.
				  *clock-tick*
				  comment
				  (snapshot-ancillary-info))
			    (make-random-state)
			    (safe-copy-schema-state)
			    (funcall microworld-snapshotter-function)))))
  (values))

(defun SUMMARIZE-SNAPSHOTS (&key (from 0)
			    (end (number-of-snapshotted-world-states))
			    (stream t)
			    (verbose nil)
			    (heading t)
			    extra-fn		; Called with args of the snapshot being reported for that line, and all the args in extra-fn-args.
			    extra-fn-args)
  (unless *snapshotted-world-states*
    (format *error-output* "~&No snapshots yet.~&")
    (return-from summarize-snapshots (values)))
  (when heading
    (let ((top
	    (format nil "Snap  Clock ~A----Timestamp-----  ~A    ~A"
		    (if verbose
			"Total Context Result ResConj Conjs "
			"")
		    #+Genera "Patch" #-Genera ""
		    (if extra-fn
			"Additional"
			"Comment"))))
      (italic-format stream "~&~A~&" top)))
  (loop for snapshot in *snapshotted-world-states*
	for counter from 0
	do (when (and (<= from counter)
		      (< counter end))
	     (with-snapshot-destructuring snapshot
	       random-state microworld-state metering-state	; Ignored.
	       (let ((verbose-results
		       (if verbose
			   (multiple-value-bind (total context result result-conj bare)
			       (schema-state->tallied-schema-types schema-state)
			     total bare		; Ignored.
			     (let ((conjs (length (schema-state->conjs-as-list schema-state))))
			       (format nil "~4D   ~4D   ~4D   ~4D   ~4D   "
				       total context result result-conj conjs)))
			   "")))
		 (format stream "~&~2D  ~6D  ~A~S  ~S  ~S~&"
			 counter
			 clock-tick
			 verbose-results
			 timestamp
			 #+Genera (subseq schema-system-version 0 2)
			 #-Genera schema-system-version	; If non-Genera, this'll be NIL by definition.  Print it anyway (*sigh*).
			 (if extra-fn
			     (apply extra-fn snapshot extra-fn-args)
			     comment))))))
  (values))

(defun-exported RESTORE-WORLD-STATE (microworld-restoration-function &optional snapshot-spec)
  (let ((snap (snapshot-from-spec snapshot-spec)))
    (with-snapshot-destructuring snap
      schema-system-version timestamp		; Ignored.
      clock-tick comment			; Ignored.
      (setf *random-state* (make-random-state random-state))	; (Without the MAKE-RANDOM-STATE, we'd side-effect the stored state---copy it!)
      (safe-restore-schema-state schema-state)
      (restore-ancillary-info metering-state)
      (funcall microworld-restoration-function microworld-state)))
  (values))

;;; Handy status report.
(defun SHOW-SCALARS (&key (stream t) (scalars *schema-state-scalars*))
  (loop for scalar in scalars
	do (format stream "~&~S = ~S~&"
		   scalar
		   (symbol-value scalar)))
  (values))

;;;; Dumping and reloading random numbers correctly (finally!).  This stuff should be
;;;; instantly obsolete once I start running 8.3.

#+Genera
(defun LOAD-DUMPABLE-RANDOM-STATE (size seed pointer-1 pointer-2 array-contents)
  ;; Stolen from CLI::RANDOM-STATE in System 435.31, but with explicit packages to
  ;; avoid the current massive confusion between CLI and FCLI.
  ;; I can't just use (CLOS:METHOD MAKE-LOAD-FORM (FUTURE-COMMON-LISP:RANDOM-STATE))
  ;; [from System 435.31] (which calls into FCLI::CONSTRUCT-RANDOM-STATE) because that
  ;; dumps something that references 'CL:ART-FIXNUM instead of 'SYS:ART-FIXNUM, because
  ;; CLI::RANDOM-STATE says 'FIXNUM below instead of 'SYS:ART-FIXNUM as I have it here.
  ;; I suppose I could just patch the definition of that function, but I _still_ don't trust
  ;; the packages here; this way, if I don't have my code loaded, trying to load the dumped
  ;; file will just explode because SCHEMA::LOAD-DUMPABLE-RANDOM-STATE won't be defined.
  (fcli::construct-random-state
    :array (make-array size :element-type 'fixnum :initial-contents array-contents)
    :size size :seed seed :pointer-1 pointer-1 :pointer-2 pointer-2))

#+Genera
(defun MAKE-DUMPABLE-RANDOM-STATE (state)
  `(load-dumpable-random-state
     ,(fcli::random-size state)
     ,(fcli::random-seed state)
     ,(fcli::random-pointer-1 state)
     ,(fcli::random-pointer-2 state)
     ',(zl:listarray (fcli::random-array state))))

#+Genera
(si:allow-redefinition '(clos:method clos::make-load-form (future-common-lisp:random-state)))

#+Genera
(clos:defmethod fcli::make-load-form ((state fcli::random-state))
  (make-dumpable-random-state state))

;;;; FASD-form dumping/loading.

(defvar *SNAPSHOT-FASD* nil)			; Temporary for loading/dumping.

#+Genera
(defun MAKE-SCHEMA-SYSTEM-VERSION-DUMPABLE (snapshot)
  ;; System objects have no FASD-FORM, so rather than get into that sticky business,
  ;; we cheat and just dump & reload its name.
  (with-complete-snapshot-destructuring snapshot
    `(((,major-version ,minor-version ,status ,(sct:system-name system) ,system-branch ,eco-level)
       ,timestamp ,clock-tick ,comment ,metering-state)
      ,random-state ,schema-state ,microworld-state)))

#-Genera
(defun MAKE-SCHEMA-SYSTEM-VERSION-DUMPABLE (snapshot)
  snapshot)

#+Genera
(defun MAKE-SCHEMA-SYSTEM-VERSION-LOADABLE (snapshot)
  (with-complete-snapshot-destructuring snapshot
    `(((,major-version ,minor-version ,status ,(sct:find-system-named system) ,system-branch ,eco-level)
       ,timestamp ,clock-tick ,comment ,metering-state)
      ,random-state ,schema-state ,microworld-state)))

#-Genera
(defun MAKE-SCHEMA-SYSTEM-VERSION-LOADABLE (snapshot)
  snapshot)

;;; Here's the model I'm using for this.  Empirically, I have the following table,
;;; where the top line is the number of schemas, and the bottom line is the size
;;; in FEPFS blocks of the resulting FASD (these are at iteration 1000, 2000, ..., 10000):
;;; (122 317 454 691  953  1333 1665 1926 2437 2992)	; Schemas
;;; (315 612 823 1182 1581 2156 2661 3064 3855 4700)	; Sizes.
;;; [I also know that a one-schema run has a size of 143 blocks.]
;;; My trusty calculator yields the following values for this data (not including the
;;; one-schema run):  Corr .99999476253 (so close to 1 it's ludicrous!).
;;; Coviance 1398786.  Linear regression:  y-intercept 125.94, slope 1.528.
;;; This would tend to imply that a one-schema run would be only 126 blocks, not
;;; 143, so I'll just overestimate by a constant factor and declare the y-intercept
;;; to be 150 (wasting at most 25 blocks, and, since we get them back when the file
;;; is closed [it's truncated to the correct length], this can only lose if we're within
;;; 25 blocks of running out of FEPFS space anyway, which isn't much of a worry).
;;; The above figures for slope and y-intercept are in blocks, of course, not 16B's,
;;; so we have to multiply each of them by 1280/2, or 640, since that's what our
;;; SI:*PREALLOCATE-FOR-FEP-FASD* hack requires.
(defparameter *GUESS-AT-FASD-SIZE-SLOPE* 1.53)	; ... *MAXIMUM* at 3000 or whatever) can reset these as appropriate.
(defparameter *GUESS-AT-FASD-SIZE-INTERCEPT* 150)

;;; This is only accurate for Genera, but non-Genera implementations will ignore its
;;; result anyway, since it's used by PREALLOCATING-FOR-DUMP below, which is a
;;; no-op in other implementations.
(defun GUESS-AT-FASD-SIZE (&optional (number-of-schemas *schema-number*))
  (round (* (+ *guess-at-fasd-size-intercept*
	       (* (or number-of-schemas *schema-number*)  ; [We may get called with an explicit NIL, in which case do the right thing...]
		  *guess-at-fasd-size-slope*))
	       640)))				; Conversion from FEPFS blocks to 16B's.

(defmacro DUMPING-SCHEMAS ((&optional number-of-schemas) &body body)
  `(with-dump-parameters
     (preallocating-for-dump (guess-at-fasd-size ,number-of-schemas)
       ,@body)))

#+(or Genera LispWorks)				; Other implementations may not be able to dump at all (e.g., MCL).
(defun DUMP-SNAPSHOT-FASD (path &optional (number-of-schemas *schema-number*))
  (dumping-schemas (number-of-schemas)
    (dump-variables path *snapshot-fasd*)))

;;; Until Schema 6.13, 6 Jul 93 03:53, this was missing the LIST call, hence it dumped singleton
;;; snapshots that were only readable by (the hastily-written) APPEND-SINGLETON-SNAPSHOT.
#+(or Genera LispWorks)
(defun DUMP-ONE-SNAPSHOT (snap path)
  (setf *snapshot-fasd* (list (make-schema-system-version-dumpable snap)))
  (dump-snapshot-fasd path))

#-Genera
(defun DUMP-ONE-SNAPSHOT (snap path)
  snap path					; Ignored.
  (unimplemented))

;;; This guy isn't called.  It kinda assumes that setting *SNAPSHOT-FASD* is all there is
;;; to the story, which really isn't the case.  Left around for historical interest, I guess.
#+(or Genera LispWorks)
(defun LOAD-ONE-SNAPSHOT (path)
  (setf *snapshot-fasd* nil)			; Just in case it aborts somehow, let's not leave the old one around!
  (load path)
  *snapshot-fasd*)

#+(or Genera LispWorks)
(defun DUMP-ALL-SNAPSHOTS (path &key
			   (query t)
			   (include-list t)
			   (specific-numbers))
  (assert (or (null specific-numbers)
	      (every #'(lambda (elt)
			 (integerp elt)
			 (<= 0 elt (number-of-snapshotted-world-states)))
		     specific-numbers)))
  (setf path (pathname path))
  (let ((dump-numbers
	  (or specific-numbers
	      (loop for number from 0 below (number-of-snapshotted-world-states)
		    collect number))))
    (when query
      (format t "~&~:[Current~;Selected~] snapshots:~&"
	      specific-numbers)
      (loop for number in dump-numbers
	    do (summarize-snapshots :from number :end (1+ number)))
      (unless (y-or-n-p "Dump? ")
	(format t "~&Not dumped.~&")
	(return-from dump-all-snapshots (values))))
    (when include-list
      (format t "~&Dumping contents list...~&")
      (with-open-file (out			; Short so it'll work with FEPFS without having to define a canonical type & translations, etc.
			#+Genera (scl:send path :new-type "LIST")
			#-Genera (unimplemented)     ; %%% Isn't there some way to do this without laboriously calling MAKE-PATHNAME?
			:direction :output)
	(loop for number in dump-numbers
	      do (summarize-snapshots :from number :end (1+ number) :stream out))))
    (format t "~&Dumping ~D snapshot~:P...~&"
	    (length dump-numbers))
    (setf *snapshot-fasd*
	  (mapcar #'make-schema-system-version-dumpable
		  (loop for number in dump-numbers
			collect (nth number *snapshotted-world-states*))))
    (dump-snapshot-fasd path)))

(defun COLD-BOOT-MAYBE-INIT (init-query)
  (unless (aref *schema-array* 0)		; If NIL, we're never ever inited since booting.
    (let ((init?
	    (or (not init-query)		; If we're not supposed to query, just charge on ahead and do it.
		(yes-or-no-p "~&Initialization has never happened since cold-boot.~&~
                                It is almost certainly a mistake to load a snapshot~&~
                                until this has happened, since many other things will~&~
                                require initialization that snapshot loading will not~&~
                                provide.  Do a quick init and then load the snapshots? "))))
      (when init?
	(just-init))
      (format t "~&~:[Not initialized.~;Initialized.~]~&" init?)))
  (values))

;;; Remember that you have to use, e.g., RESTORE-EYEHAND after running this to
;;; make one of the loaded snapshots actually _be_ the current state...
#+(or Genera LispWorks)
(defun LOAD-ALL-SNAPSHOTS (path &key (load-query t) (init-query t))
  (when (and load-query *snapshotted-world-states*)
    (format t "~&The following currently-loaded snapshots will be smashed:~&")
    (summarize-snapshots)
    (unless (yes-or-no-p "Load? ")
      (format t "~&Not loaded.~&")
      (return-from load-all-snapshots (values))))
  (cold-boot-maybe-init init-query)
  (setf *snapshot-fasd* nil)
  (load path)
  (setf *snapshotted-world-states*
	(mapcar #'make-schema-system-version-loadable *snapshot-fasd*))
  (format t "~D snapshot~:P loaded:~&"
	  (number-of-snapshotted-world-states))
  (summarize-snapshots))

;;; Remember that you have to use, e.g., RESTORE-EYEHAND after running this to
;;; make one of the loaded snapshots actually _be_ the current state...
#+(or Genera LispWorks)
(defun APPEND-ALL-SNAPSHOTS (path &key (load-query t) (init-query t))
  (when load-query
    (cond (*snapshotted-world-states*
	   (format t "~&Appending to the following ~D loaded snapshot~:P:~&"
		   (number-of-snapshotted-world-states))
	   (summarize-snapshots))
	  (t
	   (format t "~&No previously-loaded snapshots; append will act like load.~&")))
    (unless (yes-or-no-p "Load? ")
      (format t "~&Not loaded.~&")
      (return-from append-all-snapshots (values))))
  (cold-boot-maybe-init init-query)
  (setf *snapshot-fasd* nil)
  (load path)
  (setf *snapshotted-world-states*
	(append *snapshotted-world-states*
		(mapcar #'make-schema-system-version-loadable *snapshot-fasd*)))
  (format t "~D snapshot~:P loaded (now ~D total).  Current snapshots:~&"
	  (length *snapshot-fasd*)
	  (number-of-snapshotted-world-states))
  (summarize-snapshots))

;;; &&& This function is TEMPORARY until anything dumped by DUMP-ONE-SNAPSHOT
;;; &&& before Schema 6.13 is no longer around in any filesystem.  *sigh* (Before
;;; &&& Schema 6.13, it dumped the snapshot itself, instead of a singleton list of the
;;; &&& snapshot itself.)
#+(or Genera LispWorks)
(defun APPEND-SINGLETON-SNAPSHOT (path &key (load-query t) (init-query t))
  (when load-query
    (cond (*snapshotted-world-states*
	   (format t "~&Appending to the following ~D loaded snapshot~:P:~&"
		   (number-of-snapshotted-world-states))
	   (summarize-snapshots))
	  (t
	   (format t "~&No previously-loaded snapshots; append will act like load.~&")))
    (unless (y-or-n-p "Load? ")
      (format t "~&Not loaded.~&")
      (return-from append-singleton-snapshot (values))))	; [This one had to get changed, too...]
  (cold-boot-maybe-init init-query)
  (setf *snapshot-fasd* nil)
  (load path)
  (setf *snapshotted-world-states*
	(append *snapshotted-world-states*
		(list (make-schema-system-version-loadable *snapshot-fasd*))))	; &&&!!!&&& This is the changed line.
  (format t "~D snapshot~:P loaded (now ~D total).  Current snapshots:~&"
	  (length *snapshot-fasd*)
	  (number-of-snapshotted-world-states))
  (summarize-snapshots))

;;;; Building up a library of checkpoint snapshots at various points in a run.

#+Genera
(defun QUICK-GC-REPORT (&optional (stream t))
  (multiple-value-bind (committed-free-space free-space immediate-committed-free-space)
      (si:gc-get-committed-free-space)
    committed-free-space immediate-committed-free-space	; Ignored.
    (format stream "~&Free space ~:D.~&"
	    free-space))
  (values))

#+Genera
(defun FAST-WAIT-FOR-GC-COMPLETE ()
  (scl:process-wait "GC completion wait"
    #'(lambda ()
	(not si:*ephemeral-gc-in-progress*)
;       (string-equal (scl:send si:gc-process :whostate)
;                     "Await ephemeral full")
	)))

#+Genera
(defun WAIT-FOR-GC-COMPLETE ()
  ;; Theoretically, I shouldn't need this function, but it seems like the GC sometimes falls behind
  ;; and never catches up until I let it.  This is peculiar, given that it runs at priority 5, but...
  (sleep 1 :sleep-reason "GC prewait")		; Give it a chance to get started.
  (fast-wait-for-gc-complete)
  (sleep 1 :sleep-reason "GC postwait")		; Superstition.
  (values))

#+Genera
(defun WAIT-FOR-AND-SUMMARIZE-GC (&optional (stream t))
  (wait-for-gc-complete)
  (quick-gc-report stream))

#+Genera					; ... because I'm using :DIRECTORY-PATHNAME-AS-FILE.
(defun ERR-IF-SNAPSHOT-DIRECTORY-NONEXISTENT (spp)	; Doesn't return any useful value.
  (let ((dir (scl:send (pathname spp) :directory-pathname-as-file)))
    (unless (probe-file dir)
      (error "~A doesn't exist, so snapshotting there won't be possible." dir))))

#-Genera
(defun ERR-IF-SNAPSHOT-DIRECTORY-NONEXISTENT (spp)	; Doesn't return any useful value.
  spp						; Ignored.
  (unimplemented))

(defvar *ABORT-PENDING-CHECKPOINT* nil)

(defun ABORT-PENDING-CHECKPOINT ()
  (setf *abort-pending-checkpoint* t))

(defun RESET-ABORT-PENDING-CHECKPOINT ()
  (when *abort-pending-checkpoint*
    (format t "~&Warning:  ~S was ~S.  Resetting it...~&"
	    '*abort-pending-checkpoint* *abort-pending-checkpoint*)
    (setf *abort-pending-checkpoint* nil)))

(def-debugging-switch CHECKPOINT-BUNCHES-AWAIT-GC t)

;;; NB!  Using ' and not #' so I can pick up changes to these functions immediately.
;;; (You'd think that's rare, but I just got screwed when adding :LOG-ENABLED functionality.)
(defparameter *DEFAULT-RUNNER-FN* 'run-eyehand)
(defparameter *DEFAULT-SNAPSHOT-FN* 'save-eyehand)

(defun CHECKPOINT-BUNCHES (&key (bunch-size 1000)
			   hours-between-checkpoints	; If set, calls the runner with REAL-TIME-NOT-TO-EXCEED set to 3600 times this.
			   (start-new-run (start-of-run?))	; Only for the FIRST call to the runner-fn, not for each bunch!
			   arbitrary-stop-fn
			   (number-of-bunches most-positive-fixnum)	; Until we run off the end of an array.
			   (never-snapshot)	; So I can use this as a convenient way to run just a couple of steps & stop.
			   (never-dump)		; Assuming that snapshotting is enabled, this punts the dump, but not the prior snapshot.
			   (snapshot-on-bounds-exceeded (not never-snapshot))	; Do a snapshot when we run out of schema (or conj etc) space. ...
						; [Note that we still _won't_ snapshot/dump if NEVER-SNAPSHOT/NEVER-DUMP are set.]
			   (runner-fn *default-runner-fn*)
			   (snapshot-fn *default-snapshot-fn*)
			   (snapshot-pathname-prefix "FEP1:>")
			   (log-enabled t)
			   (log-name-prefix "eyehand-bunches")
			   (log-directory *default-log-directory*))
  (unless never-snapshot
    (err-if-snapshot-directory-nonexistent snapshot-pathname-prefix))
  (reset-sync-stop)
  (reset-abort-pending-checkpoint)
  (loop with continue? = t
	repeat number-of-bunches
	while (and continue? (not *synchronous-stop*))	; If SYNC-STOP was set, don't let the runner function autoreset it!
	do #+Genera (when *checkpoint-bunches-await-gc-enabled*
		      (wait-for-and-summarize-gc))
	   ;; The idea here is just to dismiss the error if it's a bounds-exceeded error,
	   ;; let snapshotting happen, and then quit.  Otherwise, blow out as usual.
	   (condition-case-if snapshot-on-bounds-exceeded (err)
		(funcall runner-fn
			 bunch-size
			 :real-time-not-to-exceed (when hours-between-checkpoints (* hours-between-checkpoints 3600))
			 :start-new-run start-new-run
			 :arbitrary-stop-fn arbitrary-stop-fn
			 :log-enabled log-enabled
			 :log-name-prefix log-name-prefix
			 :log-directory log-directory)
	      ;; I'll deal with preserving the actual error if I need to, with :bug-report-description.
	      ;; Note that ERROR signals SYS:FATAL-ERROR in Genera.  I haven't yet bothered to make
	      ;; a more appropriate condition for things to signal when they run out of room.  If we
	      ;; get any other sort of error, I want to inspect it personally.
	      (sys:fatal-error
		(setf continue? nil)
		(format *error-output* "~&*** Got an error:  ~A~&" err)))	; For Genera and multiline errors, could prettify this with ~-> and ~...
	   (setf start-new-run nil)		; Just in case it started out T, we don't want to run the next bunch from ground zero again!
	   (unless (or never-snapshot
		       *abort-pending-checkpoint*)
	     (funcall snapshot-fn
		      (format nil "Bunch run to ~A." *clock-tick*))
	     (unless never-dump
	       (dump-one-snapshot
		 (most-recent-snapshot)
		 (format nil #+Genera "~A~A.ibin"    ; Ick.  Should make this a function & put it in GENERA-ISMS, except that there's no portable ...
			 #+LispWorks  "~A~A.mfasl"   ; ... way to just change the type without the whole MAKE-PATHNAME route.  *sigh*
			 snapshot-pathname-prefix
			 *clock-tick*)))))
  (values))

;;; End of file.
