;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

(in-package :schema)

;;; Many things here are exported, even though (as of Schema 10.0) many of them
;;; are used only in the schema mechanism.  Since they might be useful in
;;; implementing a microworld, or in testing one (if my current reader package isn't
;;; SCHEMA:), the ones that are likely to be useful are exported.

;;; Note also that not everything here is, strictly speaking, a "Genera-ism".  Some of
;;; them are also peculiar to, e.g., LispWorks.  Someday, this whole file should probably
;;; just get renamed IMPLEMENTATION-SPECIFIC or something (probably before the next
;;; recompile).

;;; Make Genera know that these things are function-defining-forms.
;;; This actually isn't strictly correct in all the ways I'm using this:
;;; many calls are really defining variables in one way or another, and
;;; hence the DEFUN type isn't right.  However, it appears to work anyway.
;;; (Good news for those that define variables & functions all at once!)
#+Genera
(defmacro DEF-DEFINER (name)
  `(si:defprop ,name defun zwei:definition-function-spec-type))

#-Genera
(defmacro DEF-DEFINER (name)
  `(ignore ',name))

;;; Handy for making things available to others.

(defmacro DEFMACRO-EXPORTED (name pattern &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,pattern ,@body)))
(def-definer DEFMACRO-EXPORTED)			; This is itself a function-defining-form.
(export    '(DEFMACRO-EXPORTED))		; Make the export-creating macro exported itself, so I can easily use it in other packages.

;;; More stuff to make it easy to tell Genera that these are function-defining-forms.
(defmacro-exported DEFMACRO-DEFINER (name pattern &body body)
  `(progn
     (defmacro ,name ,pattern ,@body)
     (def-definer ,name)))
(def-definer DEFMACRO-DEFINER)

(defmacro-exported DEFMACRO-EXPORTED-DEFINER (name pattern &body body)
  `(progn
     (defmacro-exported ,name ,pattern ,@body)
     (def-definer ,name)))
(def-definer DEFMACRO-EXPORTED-DEFINER)

;;; Actual function-defining-forms.
(defmacro-exported-definer DEFUN-EXPORTED (function-spec lambda-list &body body)
  `(progn
     (export ',function-spec)
     (defun ,function-spec ,lambda-list ,@body)))

(defmacro-exported-definer DEFVAR-EXPORTED (name &rest args)
  `(progn
     (export ',name)
     (defvar ,name ,@args)))

(defmacro-exported-definer DEFCONSTANT-EXPORTED (name &rest args)
  `(progn
     (export ',name)
     (defconstant ,name ,@args)))

;;;; Package & symbol stuff.

;;; This is a useful idiom.  Note that this definition is identical (i.e., copied from)
;;; Genera's implementation of SCL:DEFSUBST, hence it'll perform identically.
(defmacro-exported-definer DEFSUBST (function lambda-list &body body)
  `(progn (cl:proclaim '(cl:inline ,function))
	  (defun ,function ,lambda-list ,@body)))

(defmacro-exported-definer DEFSUBST-EXPORTED (function lambda-list &body body)
  `(progn
     (export ',function)
     (defsubst ,function ,lambda-list ,@body)))

;;; +++ Interning things.   Not strictly a Genera-ism, but useful for macros, and must
;;; +++ be defined pretty early.

(defsubst-exported INTERN-UP (string &optional pkg)
  (intern (string-upcase string) pkg))

;;; This interns in whatever package the current file is in.
(defmacro-exported INTERN-FORMAT (format-string &rest format-args)
  `(intern-up (format nil ,format-string ,@format-args)))

;;; This takes an explicit package.
(defmacro-exported INTERN-FORMAT-PKG (pkg format-string &rest format-args)
  `(intern-up (format nil ,format-string ,@format-args) ,pkg))

;;; This is defined separately for Genera and non-Genera for two reasons.  First,
;;; Genera's version of this includes advice to the compiler & editor.  Second, SETF
;;; of FDEFINITION doesn't appear to be in Genera (at least, System 435; dunno about
;;; 8.3), even though CL claims it should be supplied.  (Actually, what's going on here
;;; is that FDEFINITION _itself_ is only in SCL when it should be in CL; this means that
;;; the SETF expansion screws up, even though I've DEFF'ed FDEFINITION to
;;; SCL:FDEFINITION, and hence I'd have to conditionally mention SCL:FDEFINITION
;;; instead of FDEFINITION when defining DEFF.)  I suppose I could just make this
;;; produce a macro (with the new function name) that expands into a call to the old
;;; function name, but I don't feel like it.
#+Genera
(eval-when (compile load eval)
(scl:deff DEFF 'scl:deff)
(def-definer DEFF)				; I'm not entirely sure that DEFUN semantics (from DEF-DEFINER) are right here, but it seems to work...
)						; EVAL-WHEN.
#-Genera
(defmacro DEFF (function definition)
  `(setf (fdefinition ',function) ,definition))
(eval-when (compile load eval)
(export '(deff))				; Either way, export it.
)						; EVAL-WHEN.

(defmacro-definer DEFF-EXPORTED (function definition)	; See comments above at (DEF-DEFINER DEFF).
  `(progn
     (export ',function)
     (deff ,function ,definition)))

;;; *SIGH*.  Maybe this is fixed in a more recent world than 435?
;;; Not needed (in Genera) in my definition of DEFF, so it's okay to define it after DEFF gets defined.
#+Genera
(deff-exported FDEFINITION 'scl:fdefinition)

;;; +++
;;; Ignoring things in a portable way.  Why is this so hard?
;;; For now, we only do this for HCL.  Maybe MCL & others (e.g., all but Genera)
;;; need it, but I'm not gonna try that until I have to.

;;; I don't think that this works at all.
; #+Lispworks
; (defmacro-exported IGNORE (&rest to-be-ignored)
;   `(declare (ignore ,@to-be-ignored)))

;;; This probably works, but generates extra code and conses at runtime!
#+LispWorks
(proclaim '(inline ignore))
#+LispWorks
(defun IGNORE (&rest ignored)
  ignored)
;;; ---

;;; Easier-to-read symbology.  If this is run not under Genera, let's hope that
;;; PREFIX is a literal string and hence won't generate a not-used compiler warning.
;;; If that turns out to be a problem, I can probably change the expansion to
;;; something like `(progn (ignore prefix) (gensym)).
(defmacro-exported GENSYMBOL (prefix)
  #+Genera `(sys:gensymbol ,prefix)
  #-Genera `(progn ,prefix (gensym)))		; PREFIX ignored.

;;;; Dealing with conditions.

#+Genera
(scl:deff condition-case 'scl:condition-case)
#+Genera
(zwei:defindentation (condition-case 1 4 2 2))

#+Genera
(defmacro-exported CONDITION-CASE-IF (cond (&rest varlist) form &rest clauses)
  (declare (zwei:indentation 2 4 3 2))
  `(scl:condition-case-if ,cond ,varlist ,form ,@clauses))

;;; The idea here is to transform Genera's CONDITION-CASE into
;;; ANSI's HANDLER-CASE.  (Why not just use HANDLER-CASE?  Good question!
;;; In part, it's because I also need CONDITION-CASE-IF, and I have to build
;;; _that_ by hand anyway, since there's no HANDLER-CASE-IF.  There are
;;; also package issues to worry about anyway, since it's in CONDITIONS:
;;; in Genera.)  The problem here is that CONDITION-CASE looks like
;;; (CONDITION-CASE (condition)
;;;     form
;;;   (handler-1 body...))
;;; where BODY gets CONDITION bound for it, whereas HANDLER-CASE looks like
;;; (HANDLER-CASE form
;;;   (handler-1 (condition) body...))
;;; and the condition object is bound per-form.  So we can certainly turn
;;; CONDITION-CASE into HANDLER-CASE, but not vice versa (because there's
;;; no requirement that two handlers in a HANDLER-CASE bind the same varlist).

#-Genera
(defmacro-exported CONDITION-CASE ((&rest varlist) form &body clauses)
  (let ((handler-case-clauses
	  (loop for (condition . body) in clauses
		collect `(,condition ,varlist ,@body))))
    `(progn					; PROGN here is just to make the macroexpansion easier to debug via C-M.
       (handler-case ,form
	 ,@handler-case-clauses))))

#-Genera
(defmacro-exported CONDITION-CASE-IF (cond (&rest varlist) form &body clauses)
  `(if ,cond
       (condition-case ,varlist ,form ,@clauses)
       ,form))

;;; This is the Genera equivalent, for the moment (e.g., debugging).
; (defmacro NEW-CONDITION-CASE ((&rest varlist) form &body clauses)
;   #+Genera (declare (zwei:indentation 1 4 2 2))
;   (let ((handler-case-clauses
; 	  (loop for (condition . body) in clauses
; 		collect `(,condition ,varlist ,@body))))
;     `(progn					; PROGN here is just to make the macroexpansion easier to debug via C-M.
;        (conditions:handler-case ,form
; 	 ,@handler-case-clauses))))
; 
; (new-condition-case (condition)
;      (some-form)
;    (some-condition form1 form2)
;    (some-other-condition form3 form4))
; 
; (defmacro NEW-CONDITION-CASE-IF (cond (&rest varlist) form &body clauses)
;   (declare (zwei:indentation 2 4 3 2))
;   `(if ,cond
;        (new-condition-case ,varlist ,form ,@clauses)
;        ,form))
; 
; (new-condition-case-if some-cond (condition)
;      (some-form)
;    (some-condition form1 form2)
;    (some-other-condition form3 form4))

;;; Genera puts ANSI conditions into CONDITIONS: (and normal Genera conditions into
;;; SCL:, but we won't worry about there here).  I probably should have made things
;;; inherit from CONDITIONS (or FUTURE-COMMON-LISP) or somesuch in Genera, and
;;; things might have worked, but it's a little late in the game to be playing such
;;; potentially dangerous games with the package system, so we'll just fake it here.
#+Genera
(eval-when (compile load eval)
(scl:deff define-condition 'conditions:define-condition)
(scl:deff signal 'conditions:signal)
;;; ERROR and CERROR can't be handled the same way, because of conflicts
;;; between the native Genera versions and these.  (You'd think the package
;;; system would deal with this, but it doesn't; instead, I wind up redefining
;;; them away from the Genera versions, and then attempting to use them
;;; causes the stack to overflow.)
(scl:deff schema-cerror 'conditions:cerror)
(scl:deff schema-error  'conditions:error)
)						; EVAL-WHEN.

#-Genera
(defun SCHEMA-CERROR (continue-string datum &rest arguments)
  (apply #'cerror continue-string datum arguments))

#-Genera
(defun SCHEMA-ERROR (datum &rest arguments)
  (apply #'error datum arguments))

;;;; Keywords & arglists.

#+Genera
(defsubst-exported REM-KEYWORDS (list keywords)
  (si:rem-keywords list keywords))

#-Genera
(defsubst-exported REM-KEYWORDS (list keywords)
  (loop for (value indicator) on list by #'cddr
	unless (member value keywords)
	  nconc (list value indicator)))

(defsubst-exported KEEP-KEYWORDS (list keywords)
  (loop for (value indicator) on list by #'cddr
	when (member value keywords)
	  nconc (list value indicator)))

;;;; Progress reports.

#+Genera
(deff-exported DOTIMES-NOTING-PROGRESS 'tv:dotimes-noting-progress)

#-Genera
(defmacro-exported DOTIMES-NOTING-PROGRESS ((var countform name
						 &optional progress-note-variable process)
					    &body body)
  `(progn
     ,name ,progress-note-variable ,process	; Ignored.
     (dotimes (,var ,countform)
       ,@body)))

#+Genera
(deff-exported NOTING-PROGRESS 'tv:noting-progress)
#+Genera
(zwei:defindentation (noting-progress 0 3 1 1))

#-Genera
(defmacro-exported NOTING-PROGRESS ((name &optional (variable process)) &body body)
  `(progn
     ,name ,variable ,process			; Ignored.
     ,@body))

#+Genera
(deff-exported NOTE-PROGRESS 'tv:note-progress)

#-Genera
(defsubst-exported NOTE-PROGRESS (numerator &optional denominator note)
  numerator denominator note			; Ignored.
  (values))

;;;; Random stuff.

(defmacro WITHOUT-FLOATING-UNDERFLOW-TRAPS (&body body)	; Presumably, only the schema mechanism does floating-point, so I don't need to export this.
  #-Genera `(progn ,@body)
  #+Genera `(sys:without-floating-underflow-traps ,@body))

;;; Make it easy to find these guys by giving them a function name.
(defun-exported UNIMPLEMENTED (&optional (error-format-string "Sorry, this isn't implemented yet.")
			       &rest error-format-args)
  (apply #'error error-format-string error-format-args))

(defmacro-exported LISTARRAY (array &optional limit)
  #+Genera `(zl:listarray ,array ,limit)
  ;; I dunno...  should I just make LIMIT be optionally set to (LENGTH ARRAY) and not special-case?
  ;; The HCL versions of these two branches run in apparently-identical time (8ms/1000 elements).
  ;; Of course, we're unbundling the &OPTIONAL arg handling here, but really...
  #-Genera (if limit				
	       `(loop for i being the array-elements of ,array below limit	; Hope the compiler does a fair job of optimizing this...
		      collect i)
	       `(map 'list #'identity ,array)))

;;; I keep needing these.  (APPLY #'MAX (car <stuff>) (cdr <stuff>)) won't work for two reasons:
;;;    We may have more items in <stuff> than the architectural limit allows for function calls.
;;;    We may have a vector, not a list.
(defun-exported MAXIMIZE (sequence)
  (loop for thing in (coerce sequence 'list)
	maximize thing))

(defun-exported MINIMIZE (sequence)
  (loop for thing in (coerce sequence 'list)
	minimize thing))

;;; This is handy, too.
(defsubst-exported LISTIFY (thing)
  (if (listp thing)
      thing
      (list thing)))

;;; Why these appear to be in SCL: instead of CL:, I have no idea.  I guess neither CLtL
;;; not ANSI deemed them to be useful...  The separate definitions here (instead of
;;; just using DEFF irregardless of wwhether or not we're in Genera) are to take
;;; advantage of compiler optimizers in Genera, and to allow other implementations
;;; to do likewise, whereas I'm not sure they will with a DEFF'ed form.
#+Genera
(deff-exported TRUE  'scl:true)
#+Genera
(deff-exported FALSE 'scl:false)
#-Genera
(defsubst-exported TRUE  (&rest ignore) ignore t)
#-Genera
(defsubst-exported FALSE (&rest ignore) ignore nil)

;;; I hate having to keep calling CONCATENATE...
;;;
;;; Note that, at least in Genera, the compiler optimizes away the APPLY, so this is
;;; just as fast as a macro that does this, but can also be used functionally.  Yay!
;;;
;;; NOTE that this will explode ("not a sequence") if any of the "strings" are really
;;; characters, but that CONCATENATE has the same behavior, so using this instead
;;; of concatanate won't cause formerly working code to blow up.
(defsubst-exported STRING-APPEND (&rest strings)
  (apply #'concatenate 'string strings))

;;; Simple list-hacking.
#+Genera
(deff-exported CIRCULAR-LIST 'scl:circular-list)

#-Genera
(defsubst-exported CIRCULAR-LIST (&rest args)
  (and args
       (let ((list (copy-list args)))
	 (rplacd (last list) list)
	 list)))

;;; Putting all the symbols in some list (or some single symbol) into a particular package.
;;; Since this calls INTERN, having it not be inlined is in the noise.
(defun-exported REPACKAGE (symbol-or-list new-package &key numbers-okay err-if-nil)
  (flet ((do-it (symbol)
	   (if (numberp symbol)
	       (if numbers-okay
		   symbol
		   (error "Got ~S when numbers were not permitted." symbol))
	       (intern (symbol-name symbol) new-package))))
    (cond (symbol-or-list
	   (if (consp symbol-or-list)
	       (mapcar #'do-it symbol-or-list)
	       (do-it symbol-or-list)))
	  (t
	   (when err-if-nil			; Don't bother trying to intern NIL.
	     (error "Can't repackage NIL."))))))

#+Genera
(deff-exported LET-IF 'scl:let-if)
#+Genera
(zwei:defindentation (let-if 2 1))

#-Genera
(defmacro-exported LET-IF (cond bindings &body body)
  `(if ,cond
       (let ,bindings
	 ,@body)
       ,@body))

#+Genera
(deff-exported WITH-STANDARD-IO-ENVIRONMENT 'scl:with-standard-io-environment)	; Hmm...

;;;; Hey, FORMAT: ain't CL!

#+Genera
(deff-exported FORMAT-PRINT-LIST 'format:print-list)

;;; Stolen from SYS: IO; FORMAT.LISP's FORMAT:PRINT-LIST.
;;; Less messy interface to list-printing stuff -- but it conses

#-Genera
(defun-exported FORMAT-PRINT-LIST (destination element-format-string list
					       &optional (separator-format-string ", ")
					       (start-line-format-string "   ")
					       (tilde-brace-options ""))
  "Print the elements of list without lapping across line boundaries"
  (let ((fstring (format nil "~~~A{~~<~~%~A~~~D:;~A~~>~~^~A~~}"
			 tilde-brace-options
			 start-line-format-string
			 (length separator-format-string)	; [Was STRING-LENGTH in FORMAT:.]
			 element-format-string
			 separator-format-string)))
    (format destination fstring list)))

;;;; More FORMAT fixes.

;;; Compensates for buggy handling of ~T directives to Genera file streams, by
;;; stuffing output into a string (which works) and then emitting that to the file.
;;; More work for the EGC, but better output.  This is _only_ necessary if the
;;; format string is either variable (which means that it might be passed some string
;;; with tabbing commands in it), or is a fixed constant with tabbing commands, AND
;;; if the destination stream is not a constant NIL (e.g., not already outputting to a
;;; string).  It is, in fact, _incorrect_ to put this everywhere, since then things that
;;; use "~&" in their format strings will _always_ emit a newline, when they might
;;; not otherwise have to.  This also compensates for what _I_ think is a bug in the
;;; handling of "~&" when the output is going to a string, namely that a leading "~&"
;;; gets "lost" (the string doesn't start with a newline, since the string doesn't know
;;; where the real cursorpos is and assumes it's zero).  In this case (leading "~&"),
;;; we do an explicit :FRESH-LINE to the stream (e.g., (FORMAT STREAM "~&")) to ensure
;;; it doesn't get lost.
#+Genera					; For the above bug, and for the use of SCL:ONCE-ONLY.
(defmacro WITH-FORMAT-~&-FIXED (stream format-string &body body &environment env)
  (scl:once-only (stream format-string &environment env)
    `(progn
       (when (and (stringp ,format-string)
		  (> (length ,format-string) 1)
		  (string-equal ,format-string "~&" :end1 2))
	 (format ,stream "~&"))
       ,@body)))

#+Genera					; For the above bug, and for the use of SCL:ONCE-ONLY.
(defmacro-exported SAFE-FORMAT (stream format-string &rest format-args &environment env)
  (scl:once-only (stream format-string &environment env)
    `(with-format-~&-fixed ,stream ,format-string
       (format ,stream "~A"
	       (format nil ,format-string ,@format-args)))))

#-Genera
(defmacro-exported SAFE-FORMAT (stream format-string &rest format-args)
  `(format ,stream ,format-string ,@format-args))

;;; Easier fonts.  Since these are being run through a second FORMAT into a string,
;;; as in SAFE-FORMAT above, you don't need to call SAFE-FORMAT on the results.
;;; However, doing so will do no harm, except it'll be a tiny bit slower, of course.
;;; [Err, uhh, except for the non-Genera case, but SAFE-FORMAT is a no-op there,
;;; too, so we're still okay.]
#+Genera
(defmacro-exported BOLD-FORMAT (stream format-string &rest format-args &environment env)
  (scl:once-only (stream format-string &environment env)
    `(with-format-~&-fixed ,stream ,format-string
       (format ,stream "~'b~A~"
	       (format nil ,format-string ,@format-args)))))

#-Genera
(defmacro-exported BOLD-FORMAT (stream format-string &rest format-args)
  `(format ,stream ,format-string ,@format-args))

#+Genera
(defmacro-exported ITALIC-FORMAT (stream format-string &rest format-args &environment env)
  (scl:once-only (stream format-string &environment env)
    `(with-format-~&-fixed ,stream ,format-string
       (format ,stream "~'i~A~"
	       (format nil ,format-string ,@format-args)))))

#-Genera
(defmacro-exported ITALIC-FORMAT (stream format-string &rest format-args)
  `(format ,stream ,format-string ,@format-args))

#+Genera
(defmacro-exported BOLD-ITALIC-FORMAT (stream format-string &rest format-args &environment env)
  (scl:once-only (stream format-string &environment env)
    `(with-format-~&-fixed ,stream ,format-string
       (format ,stream "~'p~A~"
	       (format nil ,format-string ,@format-args)))))

#-Genera
(defmacro-exported BOLD-ITALIC-FORMAT (stream format-string &rest format-args)
  `(format ,stream ,format-string ,@format-args))

;;;; Idiom for printing certain objects while respecting all the various relevant output flags.

(defmacro-exported PRINT-RESPECTING-FLAGS (print-fn object)
  `(cond (*print-escape*
	  #+Genera (if scl:*print-structure-contents*
		       (sys:cl-structure-printer (typep ,object) ,object stream depth)
		       (sys:printing-random-object (,object stream :typep)
			 (,print-fn)))
	  #-Genera (progn ,object (,print-fn)))	; OBJECT ignored.
	 (t
	  (progn ,object (,print-fn)))))	; OBJECT ignored.

;;; Compensate (temporarily) for a curious problem with DESCTRUCTURING-BIND.
;;; This piece of ANSI CL doesn't seem available to me under Genera in the package
;;; I'm using, which probably means I need to inherit it from FCLI: or something.
;;; It's not a pre-8.3 bug, since it's the same there.  So presumably this can go
;;; away once I fix whatever package confusion is causing it, and certainly doesn't
;;; need to be here for other Lisp implementations.  (Hmm.  DEFF doesn't work for
;;; macros.  Is there something else I could have used, instead of the below?)
#+Genera
(defmacro-exported DESTRUCTURING-BIND (&body body)
  (declare (zwei:indentation 1 3 2 1)	; Stolen from the SCL: definition.
	   (compiler:do-not-record-macroexpansions))	  ; Ditto.
  `(scl:destructuring-bind
     ,@body))

;;;; Figuring out exactly how many bits we can use in various fields.

;;; For the moment, this is going underutilized in Genera (which has 32-bit fixna),
;;; because I don't want to change too many things at once.
(eval-when (load eval compile)
(defun-exported NUMBER-OF-BITS-IN-A-FIXNUM ()
  (1+ (integer-length most-positive-fixnum)))
)						; EVAL-WHEN.

;;; For the moment, this is "safe" in the sense that it bitches if it doesn't
;;; have at least 24 bits available, and also won't use more than 24 bits if
;;; they're available.  A "correct" implementation should handle both cases.
;;; [Note:  Genera has 32-bit fixna; MCL, 29, and Lucid (I presume), 24.]
(eval-when (load eval compile)
(defun-exported SAFE-NUMBER-OF-BITS-IN-A-FIXNUM ()
  (let ((bits (number-of-bits-in-a-fixnum)))
    #-short-fixna-okay
    (when (< bits 24)
      (error "Fewer than 24 bits per fixnum."))
    #-long-fixna-okay
    (min bits 24)
    #+long-fixna-okay
    bits))
)						; EVAL-WHEN.

;;; It's safe for this to be a DEFCONSTANT (so far), since this will only change
;;; if I decide that everything's been converted to variable-length (and the different
;;; macros will all demand that I recompile the whole system anyway).  Also, it can
;;; only change if the above is true and we're compiling on a different machine,
;;; which will again mean a recompilation.
(defconstant-exported *SAFE-FIXNUM-LENGTH* (safe-number-of-bits-in-a-fixnum))

;;;; Dumping state.

;;; +++ Making writing dumps to the FEPFS much faster, via preallocation.

;;; This number is the number of 16B's that will be written (since we're opening the
;;; stream that way).  Since the FEPFS only stores these as 8B's anyway, this means
;;; that the number of FEPFS block allocated is this number divided by 640 (640 is
;;; 1280/2).
#+Genera
(defvar SI:*PREALLOCATE-FOR-FEP-FASD* nil)

#+Genera
(si:allow-redefinition 'si:writing-bin-file-1)

;;; From SYS: L-BIN; DUMP.LISP.182.
#+Genera
si:
(defun WRITING-BIN-FILE-1 (continuation file set-load-default-p)
  (declare (sys:downward-funarg continuation))
  (declare (values binary-truename))
  (let ((bfp (bin-file-pathname file set-load-default-p)))
    (with-open-file (stream bfp
			    :direction :output :element-type '(cl:unsigned-byte 16.)
			    :estimated-length (and (variable-boundp *preallocate-for-fep-fasd*)
						   (numberp *preallocate-for-fep-fasd*)
						   (typep (scl:send bfp :translated-pathname)
							  'fs:fep-pathname)
						   (round *preallocate-for-fep-fasd*)))
      (funcall continuation stream))))

;;; +++ Hiding details of certain efficiency hacks from other implementations.

#+Genera
(deff-exported DUMP-FORMS-TO-FILE 'sys:dump-forms-to-file)

#-(or Genera LispWorks)
(defun-exported DUMP-FORMS-TO-FILE (&rest ignore)
  ignore
  (unimplemented))

#+Genera
(defmacro-exported WITH-DUMP-PARAMETERS (&body body)
  `(let ((si:*bin-dump-no-list-sharing* t))	; Uh oh!  What about the 2_11 bug?  [Seems fixed, actually... Hmm...]
     ,@body))

#-Genera
(defmacro-exported WITH-DUMP-PARAMETERS (&body body)
  `(progn
     ,@body))

#+Genera
(defmacro-exported PREALLOCATING-FOR-DUMP (guess &body body)
  `(let ((si:*preallocate-for-fep-fasd* ,guess))     ; To make dumps faster, if dumping to FEPFS.  Requires kinked SI:WRITING-BIN-FILE-1.
     ,@body))

#-Genera
(defmacro-exported PREALLOCATING-FOR-DUMP (guess &body body)
  `(progn
     ,@body))

;;; +++ I should have written these years ago...

;;; This one allows list sharing.  I'm superstitious in the case of
;;; dumping random numbers (in which I didn't disallow sharing),
;;; and maybe there was some reason for that...
;;; Call this as (DUMP-VARIABLE "pathname" *FOO*).
(defmacro DUMP-VARIABLE-SHARING (where variable)
  `(dump-forms-to-file
     ,where
     (list `(setq ,',variable ',,variable))))

;;; Call this as (DUMP-VARIABLE "pathname" *FOO*).
(defmacro DUMP-VARIABLE (where variable)
  `(with-dump-parameters			; Avoid list sharing.
     (dump-forms-to-file
       ,where
       (list `(setq ,',variable ',,variable)))))

;;; This supercedes the above, actually, but I might as well preserve the singular
;;; form for historical reasons (written all of ten minutes previously, but...).
;;; Call this as (DUMP-VARIABLES "pathname" *FOO* *BAR*).
(defmacro DUMP-VARIABLES (where &rest variables)
  `(with-dump-parameters			; Avoid list sharing.
     (dump-forms-to-file
       ,where
       (list ,@(loop for variable in variables
		     collect ``(setq ,',variable ',,variable))))))

(defmacro DUMP-VARIABLES-PREALLOCATING (guess where &rest variables)
  `(preallocating-for-dump ,guess
     (dump-variables ,where ,@variables)))

;;;; Stopping before we crash.  This can allow me to add paging in a controlled
;;;; fashion (e.g., without having to warm-boot, which would unwind my stacks).
;;;; Alas, of course, this _only_ works in Genera.  (Perhaps a more portable
;;;; version could be written if ROOM had portable entrypoints or something.)

;;; This is suitable for most things, but probably NOT for snapshotting
;;; (the original code in INVOKERS.LISP used 5, which is probably okay for that).
(defparameter *STOP-WHEN-THIS-MWORDS-FREE* 2)

;;; Allow manual shutdown.
(defvar *STOP* nil)

#+Genera
(defun TIME-TO-STOP? ()
  (or *stop*
      (when *stop-when-this-mwords-free*
	(multiple-value-bind (nil free-space nil)
	    (si:gc-get-committed-free-space)
	  (> (* *stop-when-this-mwords-free* 1e6) free-space)))))

#-Genera
(defun TIME-TO-STOP? ()
  *stop*)

;;; Keep bitching until the situation's corrected.  Don't let an errant space send us
;;; continuing off if it hasn't been fixed.  Note that this will bitch about space even
;;; if we've stopped because *STOP* was set true; I don't feel like being fancy about that.
(defun WAIT-FOR-MORE-PAGING-IF-NECESSARY (&optional dont-wait-for-gc)
  #+Genera (unless dont-wait-for-gc
	     (fast-wait-for-gc-complete))	; Don't slow down a fast caller, but at least wait if the GC is already active.
  (loop while (time-to-stop?)
	do (bold-format t "~&Less than ~D megaword~:P of paging space left.~&~
                             Add more, then hit any character to continue: "
			*stop-when-this-mwords-free*)
	   (read-char))
  (values))

;;; End of file.
