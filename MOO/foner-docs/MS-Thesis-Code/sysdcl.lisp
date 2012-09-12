;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;; Fairly special-purpose to the platforms I've compiled on.  The whole idea here is
;;; to only have to list the modules once (so I can't forget to update one of the
;;; lists), yet to do so in a manner that works with DEFSYSTEM, which is itself a
;;; macro (*sigh*).  It's "...(options)..." instead of "...options..." in the arglist to
;;; make it easier to have the editor get the indentation right.
(defmacro DEF-MULTIPLATFORM-SYSTEM (name (options) (&rest special-modules)
				    &body serial-modules)
  ;; This PROGN is only here to make it easier to see what this expands into for any given platform.
  ;; (Otherwise, it's expand into the local expansion of DEFSYSTEM, which is harder to verify correct.)
  `(progn
     (defsystem ,name
	 (,@options)
       #+Genera ,@special-modules
       #+Genera (:module-group main (:serial ,@serial-modules))
       #+Genera (:serial main)
       #+LispWorks :members
       #+LispWorks ,serial-modules
       #+LispWorks :rules
       #+LispWorks ((:in-order-to :compile :all
				  (:requires (:load :previous))))
       )))

;;; Make Genera know that this is a function-defining-form.  Since none of the rest
;;; of the system is defined yet, we can't use DEF-DEFINER or DEFMACRO-DEFINER.
#+Genera
(si:defprop DEF-MULTIPLATFORM-SYSTEM defun zwei:definition-function-spec-type)

;;; You'd think that saying (:TYPE :BINARY-DATA) would make Genera's DEFSYSTEM do the
;;; right thing and not try to read in LISP-END.O when doing M-X Find Files, but you'd be
;;; wrong.  This gross kluge piggybacks upon the original kluge somebody wrote for BFD's
;;; etc, but seems to work.
#+Genera
(eval-when (load eval compile)
  (cl:pushnew :o sct:*destination-file-types*))

;;; The actual system definition.
(def-multiplatform-system SCHEMA
			  (#+Genera (:pretty-name "Schema"
				     :default-pathname "Schema: Code;"
				     :initial-status :released
				     :distribute-sources t
				     :distribute-binaries t
				     )
			   #+LispWorks (:default-pathname (directory-namestring system::*load-pathname*))
			   )
 			  (#+Genera (:module hcl-lisp-end ("lisp-end.o")
					    (:type :binary-data)))
  ;; Packages.
  "packages"
  ;; Genera-isms.
  "genera-isms"					; Maybe rename to IMPLEMENTATION-SPECIFIC before next recompile?  [Foner 27 Mar 94.]
  ;; Basic arithmetic.
  "fix"
  "float"
  ;; Schema substrate.
  "schema-defs"
  "schema-objects"
  "schema-object-resizing"
  "schema-mechanism"
  ;; Getting subsets of items.  [Used in iterators, attention, and elsewhere.]
  "item-selection"
  ;; Iterators.
  "iterators"
  "eyehand-iterators"
  ;; Modifications to the schema substrate.
  "attention"
  ;; Goal-directed percepts and action selection.
  "goals"
  "action-selection"
  "eyehand-goal-directed-iterators-support"
  "eyehand-goal-directed-iterators"
  ;; Basic substrate for running microworlds.
  "def-microworld-toplevel"
  ;; Original microworld.
  "eyehand"
  "eyehand-toplevel"
  "eyehand-status-reporting"
; "eyehand-old-testing"
  ;; Mud microworld.
  "mudworld"
  "mudworld-toplevel"
  ;; Hamsterdam microworld.
  "hamsterdam-server-lisp-linkage"
  hcl-lisp-end					; Binary-data.  These are the actual C functions that make up the Lisp-end TCP socket.
  "hamsterdam"
  "hamsterdam-toplevel"
  ;; Analysis packages.
  "old-analysis"				; See if this is useful outside the microworld...
  "macsyma-meters"
  "random-analysis"
  "schema-set-analysis"
  ;; Demo stuff.
  "chaining"
  "chained-animation"
  "chaining-mcl"
  "chaining-to-goals"
  ;; Invokers.
  "invokers"
  )

;;; End of file.
