;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;; (Almost!) this entire file is in a large comment, so it won't compile to anything.
;;; It just preserves a bunch of forms that are useful when running experiments.

;;; This is needed too many times to leave for optional inclusion.  Furthermore, not
;;; running it has screwed me before, so as of Schema 12.81, this is called by the
;;; various snapshot-loaders (after an optional prompt) if the world has never been
;;; inited since cold boot.
(defun JUST-INIT (&key (runner-fn *default-runner-fn*)
		  (snapshot-fn *default-snapshot-fn*))
  (with-spinoff-schemas-with-recently-updated-stats-generators
    (with-ueis-history-items-all-schemas-generators
      (let ((*show-iterations* nil))
	(checkpoint-bunches
	  :log-enabled nil
	  :never-snapshot t
	  :bunch-size 3
	  :number-of-bunches 1
	  :start-new-run t
; 	  :snapshot-pathname-prefix "FEP1:>Moving-HIAS-RUS-Stats>"
; 	  :log-name-prefix "foobar"
	  :runner-fn runner-fn
	  :snapshot-fn snapshot-fn
	  )))))

#||

(with-spinoff-attentive-to-prior-tick-generators
  (checkpoint-bunches
    :start-new-run t
    :number-of-bunches 3
    :snapshot-pathname-prefix "FEP1:>New-Stats>"
    :log-name-prefix "eyehand-new-stats"
    ))

(with-spinoff-attentive-to-prior-tick-generators
  (with-ueis-attentive-to-prior-tick-generators
    (checkpoint-bunches
      :start-new-run t
      :number-of-bunches 3
      :snapshot-pathname-prefix "FEP1:>New-Att-Stats>"
      :log-name-prefix "eyehand-new-attentive-stats"
      )))

(with-spinoff-attentive-to-prior-tick-generators
  (with-ueis-changed-items-all-schemas-generators
    (checkpoint-bunches
      :start-new-run t
      :number-of-bunches 3
      :snapshot-pathname-prefix "FEP1:>New-CIAS-Stats>"
      :log-name-prefix "eyehand-cias-new-stats"
      )))

(with-spinoff-attentive-to-prior-tick-generators
  (with-ueis-all-items-dependent-schemas-generators
    (checkpoint-bunches
      :start-new-run t
      :number-of-bunches 3
      :snapshot-pathname-prefix "FEP1:>New-AIDS-Stats>"
      :log-name-prefix "eyehand-aids-new-stats"
      )))

(with-spinoff-full-crossbar-generators
  (with-ueis-all-items-dependent-schemas-generators
    (checkpoint-bunches
      :start-new-run t
      :number-of-bunches 3
      :snapshot-pathname-prefix "FEP1:>New-AIDS-Stats-Crossbar>"
      :log-name-prefix "eyehand-aids-new-stats-crossbar"
      )))

(with-spinoff-attentive-to-prior-tick-generators
  (with-ueis-all-items-dependent-schemas-generators
    (checkpoint-bunches
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>New-AIDS-Stats>"
      :log-name-prefix "eyehand-aids-new-stats"
      )))

(with-spinoff-attentive-to-prior-tick-generators
  (with-ueis-history-items-all-schemas-generators
    (checkpoint-bunches
      :bunch-size 2000
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>New-HIAS-Stats>"
      :log-name-prefix "eyehand-hias-new-stats"
      )))

;;; HIDS doesn't generate context spinoffs...  [That was old, broken HIDS!]
(with-spinoff-attentive-to-prior-tick-generators
  (with-ueis-history-items-dependent-schemas-generators
    (checkpoint-bunches
      :snapshot-on-bounds-exceeded nil
      :bunch-size 100
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>New-HIDS-Stats>"
      :log-name-prefix "eyehand-hids-new-stats-test"
      )))

(with-spinoff-schemas-with-recently-updated-stats-generators
  (with-ueis-history-items-all-schemas-generators
    (checkpoint-bunches
      :snapshot-on-bounds-exceeded nil
      :number-of-bunches 1
      :bunch-size 4000
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>New-HIAS-RUS-Stats>"
      :log-name-prefix "eyehand-hias-rus-new-stats-test"
      )))

(defun TEST-RECENCY-SETTINGS (&optional (settings '(0 1 2 5 10)))
  (let ((old-setting *SCHEMA-STATS-UPDATED-WITHIN-N-TICKS*))
    (unwind-protect				; Keep me from screwing up later.
	(loop for setting in settings
	      do (setf *SCHEMA-STATS-UPDATED-WITHIN-N-TICKS* setting)
		 (summarize-reported-parameters)
		 (with-spinoff-schemas-with-recently-updated-stats-generators
		   (checkpoint-bunches
		     :snapshot-on-bounds-exceeded nil
		     :number-of-bunches 1
		     :bunch-size 4000
		     :start-new-run t
		     :snapshot-pathname-prefix "FEP1:>Stats-Full-Crossbar-RUS>"
		     :log-name-prefix "eyehand-stats-full-crossbar-rus"
		     ))
		 )
      (setf *SCHEMA-STATS-UPDATED-WITHIN-N-TICKS* old-setting)))    
  (values))

;;; "Fixed" means "not broken", not "stationary".
(with-spinoff-schemas-with-recently-updated-stats-generators
  (with-ueis-history-items-all-schemas-generators
    (checkpoint-bunches
      :bunch-size 2000
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>Moving-Fixed-HIAS-RUS-Stats>"
      :log-name-prefix "eyehand-fixed-moving-hias-rus-stats"
      )))

;;; Call this to _just_ initialize a little bit of state...
(with-spinoff-schemas-with-recently-updated-stats-generators
  (with-ueis-history-items-all-schemas-generators
    (checkpoint-bunches
      :never-snapshot t
      :bunch-size 3
      :number-of-bunches 1
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>Moving-HIAS-RUS-Stats>"
      :log-name-prefix "eyehand-moving-hias-rus-stats"
      )))

(with-spinoff-schemas-with-recently-updated-stats-generators
  (with-ueis-interesting-items-all-schemas-generators
    (checkpoint-bunches
      :bunch-size 2000
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>Interesting-Items-RUS-Stats>"
      :log-name-prefix "eyehand-interesting-items-rus-stats"
      )))

;;; A try with new, fixed HIDS!  (And spinoffs using recently-updated-stats,
;;; not attentive-to-prior-tick...)
(with-spinoff-schemas-with-recently-updated-stats-generators
  (with-ueis-history-items-dependent-schemas-generators
    (checkpoint-bunches
      :bunch-size 2000
      :start-new-run t
      :snapshot-pathname-prefix "FEP1:>New-Fixed-HIDS-Stats>"
      :log-name-prefix "eyehand-fixed-hids-new-stats"
      )))

(defun TEST-SOME-DEPENDENCY-STUFF (&optional (bunch-size 5000)
				   (snapshot-prefix "FEP1:>New-Dependency-Tests>"))
  (let ((tests '((changed-item-numbers-most-specific-first-generator
		  schemas-with-recently-updated-stats-generator
		  changed-item-numbers-in-history-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator     ; E.g., not -IN-HISTORY.
		  "eyehand-(changed-item&rus-schema)&(changed-item&no-mod)")
		 (changed-item-numbers-most-specific-first-generator
		  mod-all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator ; E.g., not -IN-HISTORY.
		  all-item-numbers-generator
		  all-schema-numbers-generator
		  "eyehand-(changed-item&mod)&(all&all)")
		 (changed-item-numbers-most-specific-first-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator     ; E.g., not -IN-HISTORY.
		  all-item-numbers-generator
		  all-schema-numbers-generator
		  "eyehand-(changed-item&no-mod)&(all&all)")
		 (changed-item-numbers-most-specific-first-generator
		  mod-all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator ; E.g., not -IN-HISTORY.
		  changed-item-numbers-in-history-generator
		  all-schema-numbers-generator
		  "eyehand-(changed-item&mod)&(changed-item&all)")
		 (changed-item-numbers-most-specific-first-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator     ; E.g., not -IN-HISTORY.
		  changed-item-numbers-in-history-generator
		  all-schema-numbers-generator
		  "eyehand-(changed-item&no-mod)&(changed-item&all)")
		 )))
    (loop for (spinoff-item spinoff-schema ueis-item ueis-schema log) in tests
	  do (with-spinoff-schema-number-generator spinoff-schema
	       (with-spinoff-item-number-generator spinoff-item
		 (with-ueis-schema-number-generator ueis-schema
		   (with-ueis-item-number-generator ueis-item
		     (checkpoint-bunches
		       :bunch-size bunch-size
		       :start-new-run t
		       :snapshot-pathname-prefix snapshot-prefix
		       :log-name-prefix log
		       :number-of-bunches 1)))))))
  (values))

;;; Checking subset relationships.
(with-spinoff-schema-number-generator  'schemas-with-recently-updated-stats-generator
  (with-spinoff-item-number-generator  'changed-item-numbers-most-specific-first-generator
    (with-ueis-schema-number-generator 'weird-abspsdu-changed-items-with-and-without-history-generator
      (with-ueis-item-number-generator 'changed-item-numbers-in-history-generator
	(checkpoint-bunches
	  :bunch-size 5000
	  :snapshot-on-bounds-exceeded nil
	  :start-new-run t
	  :snapshot-pathname-prefix "FEP1:>CI-W-WO-History>"
	  :log-name-prefix "eyehand-changed-items-with-and-without-history"
	  :number-of-bunches 1)))))

(defun TEST-SOME-MORE-DEPENDENCY-STUFF (&optional (bunch-size 5000)
					(snapshot-prefix "FEP1:>New-Dependency-Tests>"))
  ;; Order is spinoff items, spinoff schemas, ueis items, ueis schemas, log.
  ;; Comment these out as we run and/or crash...
  ;; This isn't quite a full crossbar, because I don't want to know about every case, and because some cases
  ;; have been run already.
  (let ((tests '(
		 (changed-item-numbers-most-specific-first-generator	; CINMSF SWRUS AIN ABSPSDUCIIH
		  schemas-with-recently-updated-stats-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
		  "eyehand-cinmsf-swrus-ain-abspsduciih")
		 (changed-item-numbers-most-specific-first-generator	; CINMSF SWRUS AIN ABSPSDUCI
		  schemas-with-recently-updated-stats-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  "eyehand-cinmsf-swrus-ain-abspsduci")

		 (changed-item-numbers-most-specific-first-generator	; CINMSF ABSPSDUCI AIN ABSPSDUCI
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  "eyehand-cinmsf-abspsduci-ain-abspsduci")
		 (changed-item-numbers-most-specific-first-generator	; CINMSF ABSPSDUCI AIN ABSPSDUCIIH
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
		  "eyehand-cinmsf-abspsduci-ain-abspsduciih")

		 (changed-item-numbers-most-specific-first-generator	; CINMSF ABSPSDUCI CINIH ABSPSDUCI
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  changed-item-numbers-in-history-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  "eyehand-cinmsf-abspsduci-cinih-abspsduci")
		 (changed-item-numbers-most-specific-first-generator	; CINMSF ABSPSDUCI CINIH ABSPSDUCIIH
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  changed-item-numbers-in-history-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
		  "eyehand-cinmsf-abspsduci-cinih-abspsduciih")

		 ;;

		 (changed-item-numbers-most-specific-first-generator	; CINMSF ASN AIN ABSPSDUCIIH
		  all-schema-numbers-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
		  "eyehand-cinmsf-asn-ain-abspsduciih")
		 (changed-item-numbers-most-specific-first-generator	; CINMSF ASN AIN ABSPSDUCI
		  all-schema-numbers-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  "eyehand-cinmsf-asn-ain-abspsduci")

		 (changed-item-numbers-most-specific-first-generator	; CINMSF ASN CINIH ABSPSDUCIIH
		  all-schema-numbers-generator
		  changed-item-numbers-in-history-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
		  "eyehand-cinmsf-asn-cinih-abspsduciih")
		 (changed-item-numbers-most-specific-first-generator	; CINMSF ASN CINIH ABSPSDUCI
		  all-schema-numbers-generator
		  changed-item-numbers-in-history-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  "eyehand-cinmsf-asn-cinih-abspsduci")

		 ;;

		 (all-item-numbers-generator	; AIN ASN CINIH ABSPSDUCIIH
		  all-schema-numbers-generator
		  changed-item-numbers-in-history-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
		  "eyehand-ain-asn-cinih-abspsduciih")
		 (all-item-numbers-generator	; AIN ASN CINIH ABSPSDUCI
		  all-schema-numbers-generator
		  changed-item-numbers-in-history-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  "eyehand-ain-asn-cinih-abspsduci")

		 (all-item-numbers-generator	; AIN ASN AIN ABSPSDUCIIH
		  all-schema-numbers-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-in-history-generator
		  "eyehand-ain-asn-ain-abspsduciih")
		 (all-item-numbers-generator	; AIN ASN AIN ABSPSDUCI
		  all-schema-numbers-generator
		  all-item-numbers-generator
		  all-bare-schemas-plus-schemas-dependent-upon-changed-items-generator
		  "eyehand-ain-asn-ain-abspsduci")

		 )))
    (loop for (spinoff-item spinoff-schema ueis-item ueis-schema log) in tests
	  for stop? = (time-to-stop?)
	  when stop?
	    do (format t "~&Low memory.~&")
	  until stop?
	  do (with-spinoff-schema-number-generator spinoff-schema
	       (with-spinoff-item-number-generator spinoff-item
		 (with-ueis-schema-number-generator ueis-schema
		   (with-ueis-item-number-generator ueis-item
		     (checkpoint-bunches
		       :bunch-size bunch-size
		       :start-new-run t
		       :snapshot-pathname-prefix snapshot-prefix
		       :log-name-prefix log
		       :number-of-bunches 1)))))))
  (values))

;;; Not _quite_ the same---I'm not using the MOD stuff...
;;; Total estimate wallclock time to really run this is on the order of 2 days,
;;; and maybe 30-40 MW.
(defun RERUN-MACSYMA-PLOTTED-RUNS (&optional (bunch-size 5000))
  (flet ((run (log &optional (bunch-size bunch-size))
	   (checkpoint-bunches
	     :bunch-size bunch-size
	     :start-new-run t
	     :snapshot-pathname-prefix "FEP1:>Macsyma-Rerun>"
	     :log-name-prefix log)))
    (unless (time-to-stop?)
      (with-spinoff-changed-items-all-schemas-generators
	(run "eyehand-changed-items")))		; This'll take at least 10 hours.
    (unless (time-to-stop?)
      (with-spinoff-attentive-to-prior-tick-generators
	(run "eyehand-attentive")))		; This'll take at least 9 hours.
    (unless (time-to-stop?)
      (with-spinoff-full-crossbar-generators
	(run "eyehand-full-crossbar" 2000))))	; This'll take at least 28 hours!
  (values))

(defun TEST-VARIOUS-VF-STUFF (&optional (bunch-size 5000)
			      (snapshot-prefix "FEP1:>Interesting-VF>"))
  ;; Order is spinoff items, spinoff schemas, ueis items, ueis schemas, log.
  (let ((tests '(
		 (changed-item-numbers-most-specific-first-generator	; CINMSF SWRUS CINIH NVPIVS
		  schemas-with-recently-updated-stats-generator
		  changed-item-numbers-in-history-generator
		  non-vf-plus-interesting-vf-schemas-generator
		  "eyehand-vf-cinmsf-swrus-cinih-nvpivs")
		 (changed-item-numbers-most-specific-first-generator	; CINMSF SWRUS AIN NVPIVS
		  schemas-with-recently-updated-stats-generator
		  all-item-numbers-generator
		  non-vf-plus-interesting-vf-schemas-generator
		  "eyehand-vf-cinmsf-swrus-ain-nvpivs")
		 (changed-item-numbers-most-specific-first-generator	; CINMSF ASN AIN NVPIVS
		  all-schema-numbers-generator
		  all-item-numbers-generator
		  non-vf-plus-interesting-vf-schemas-generator
		  "eyehand-vf-cinmsf-asn-ain-nvpivs")
		 (all-item-numbers-generator	; AIN ASN AIN NVPIVS
		  all-schema-numbers-generator
		  all-item-numbers-generator
		  non-vf-plus-interesting-vf-schemas-generator
		  "eyehand-vf-ain-asn-ain-nvpivs")
		 )))
    (loop for (spinoff-item spinoff-schema ueis-item ueis-schema log) in tests
	  for stop? = (time-to-stop?)
	  when stop?
	    do (format t "~&Low memory.~&")
	  until stop?
	  do (with-spinoff-schema-number-generator spinoff-schema
	       (with-spinoff-item-number-generator spinoff-item
		 (with-ueis-schema-number-generator ueis-schema
		   (with-ueis-item-number-generator ueis-item
		     (checkpoint-bunches
		       :bunch-size bunch-size
		       :start-new-run t
		       :snapshot-pathname-prefix snapshot-prefix
		       :log-name-prefix log
		       :number-of-bunches 1)))))))
  (values))

(defun ANOTHER-TEST ()
  (test-various-vf-stuff)
  (rerun-macsyma-plotted-runs))

(defun OOPS-RERUN-MACSYMA-PLOTTED-RUNS (&optional (bunch-size 5000))
  (flet ((run (log &optional (bunch-size bunch-size))
	   (checkpoint-bunches
	     :bunch-size bunch-size
	     :start-new-run t
	     :snapshot-pathname-prefix "FEP1:>Macsyma-Rerun>"
	     :log-name-prefix log)))
    (unless (time-to-stop?)
      (with-spinoff-changed-items-all-schemas-generators
	(checkpoint-bunches
	  :bunch-size bunch-size
; 	  :start-new-run t
	  :snapshot-pathname-prefix "FEP1:>Macsyma-Rerun>"
	  :log-name-prefix "eyehand-changed-items")))
;     (run "eyehand-changed-items")))		; This'll take at least 10 hours.
    (unless (time-to-stop?)
      (with-spinoff-attentive-to-prior-tick-generators
	(run "eyehand-attentive")))		; This'll take at least 9 hours.
    (unless (time-to-stop?)
      (with-spinoff-full-crossbar-generators
	(run "eyehand-full-crossbar" 2000))))	; This'll take at least 28 hours!
  (values))

(defun OOPS-AGAIN-FULL-CROSSBAR ()
;   (with-spinoff-full-crossbar-generators
;     (checkpoint-bunches
;       :bunch-size 895
; ;     :start-new-run t
;       :snapshot-pathname-prefix "FEP1:>Macsyma-Rerun>"
;       :log-name-prefix "eyehand-full-crossbar"))
  (with-spinoff-full-crossbar-generators
    (checkpoint-bunches
      :bunch-size 2000
;     :start-new-run t
      :snapshot-pathname-prefix "FEP1:>Macsyma-Rerun>"
      :log-name-prefix "eyehand-full-crossbar"))
  )

(defun MOVING-HIAS-RUS-CHECK-FOR-DUPLICATES ()
  (with-spinoff-schemas-with-recently-updated-stats-generators
    (with-ueis-history-items-all-schemas-generators
      (checkpoint-bunches
	:bunch-size 5000
	:start-new-run t
	:snapshot-pathname-prefix "FEP1:>Moving-Fixed-HIAS-RUS-Stats>"
	:log-name-prefix "eyehand-fixed-moving-hias-rus-stats-duplicate-check"
	))))

(defun CONTINUE ()
  (oops-again-full-crossbar)
  (moving-hias-rus-check-for-duplicates))

(defun RERUN ()
  (just-init)
  (append-all-snapshots "FEP1:>Macsyma-Rerun>4000.ibin")
  (restore-eyehand 0)
  (continue))

;;; +++
;;; BIG runs.

(defmacro WITH-LARGER-LIMIT ((limit &optional (multiplier 10)) &body body)
  (multiple-value-bind (current historical)
      (limit-values limit)
    (let ((new-current (* current multiplier)))
      `(unwind-protect
	   (deflimit ,limit ,new-current ,historical)
	 ,@body
	 (deflimit ,limit ,current ,historical)))))

;;; There must be some clever way to write the macro so everybody can automatically nest
;;; (so I could just write WITH-LARGER-LIMITS), but I don't feel like figuring it out right now.
;;;
;;; It'd be really nice if, e.g., *SCHEMA-ARRAY* was declared with some sort of
;;; declaration that caused it be automatically updated if the relevant limit (in this
;;; case, *SCHEMA-MAXIMUM*) was updated via DEFLIMIT.  Then I wouldn't have to
;;; worry about this sort of stuff, and about catching all the references.  This would
;;; presumably require some new macro that recorded what limits some initialization
;;; form depended upon (for simplicity, we'd have the user specify it, rather than
;;; walking the code), and then keeping the information around for DEFLIMIT to use
;;; when it was called.  Of course, this means that a careless reevaluation of a
;;; DEFLIMIT would bash an array (because it'd be reinitialized), and even checking
;;; for same-limit re-DEFLIMIT would only help accidental recompilation, rather than
;;; changing a limit and then immediately changing your mind and changing it back.
;;; Hmm.  Still, it might be better than _this_ mess, which really requires a system
;;; recompilation if some critical limit like *SCHEMA-MAXIMUM* gets reset ('cause god
;;; _knows_ whose got their fingers on _that_...).
(defmacro WITH-ALL-LARGER-LIMITS ((&optional (multiplier 10)) &body body)
  ;; Doesn't bother enlarging item- or action-related limits.
  `(with-larger-limit (*schema-maximum* ,multiplier)
     (with-larger-limit (*conj-maximum* ,multiplier)
       (setf *conj-array*   (make-array *conj-maximum* :element-type 'conj))
       (setf *schema-array* (make-array *schema-maximum* :element-type 'schema))
       (setf *non-foveal-halo-vf-schemas*  nil)
       (setf *non-foveal-halo-vf-schemas*  (make-empty-schema-tick-vector '*non-foveal-halo-vf-schemas*))
       (setf *schema-tick-vector-positive* nil)
       (setf *schema-tick-vector-positive* (make-empty-schema-tick-vector '*schema-tick-vector-positive*))
       (setf *schema-tick-vector-negative* nil)
       (setf *schema-tick-vector-negative* (make-empty-schema-tick-vector '*schema-tick-vector-negative*))
       (setf *schema-tick-vector-context*  nil)
       (setf *schema-tick-vector-context*  (make-empty-schema-tick-vector '*schema-tick-vector-context*))
       (setf *predicted-results*      (make-state-array *predicted-results-size*))
       (setf *predicted-result-conjs* (make-state-array *predicted-results-conjs-size*))
       (setf *reliable-conj*          (make-flag-array *fixna-required-to-hold-all-conj-flags*))
       ,@body)))

;;; Calling WITH-ALL-LARGER-LIMITS _bashes arrays_ and hence can _only_ be called
;;; for a new run!  However, these arrays (and their limits) are set properly in a
;;; restored snapshot, so we don't need to bash them in that case.  (Of course, this
;;; means that they won't be _reset_ until a smaller snapshot is loaded, or we manually
;;; undo the damage...  Hmmm.)
(defun BIG-RUN-INITIAL ()
  ;; Do a run with huge limits.
  (with-all-larger-limits ()
    (big-run-continuation)))

;;; Call this (or some piece of this) after restoring a snapshot of the run.
(defun BIG-RUN-CONTINUATION ()
  (with-spinoff-schemas-with-recently-updated-stats-generators
    (with-ueis-history-items-all-schemas-generators
      (let ((*guess-at-fasd-size-slope* (* 3 *guess-at-fasd-size-slope*)))
; ; 	;; Get up to 10K iterations without snapshotting---I have all this data before...
; ; 	(checkpoint-bunches
; ; 	  :number-of-bunches 1
; ; 	  :bunch-size 10000
; ; 	  :start-new-run t
; ; 	  :snapshot-pathname-prefix "FEP1:>Big-Runs>"
; ; 	  :log-name-prefix "eyehand-cinmsf-swrus-cinih-asn-big-run"
; ; 	  )
; 	;; Now snapshot every few hours, regardless of how many iterations that is.
; 	(checkpoint-bunches
; 	  :bunch-size most-positive-fixnum	; Never checkpoint per iteration, but only by wallclock time.
; 	  :hours-between-checkpoints 8
; 	  :start-new-run nil
; 	  :snapshot-pathname-prefix "FEP1:>Big-Runs>"
; 	  :log-name-prefix "eyehand-cinmsf-swrus-cinih-asn-big-run"
	;; Special as of 5 Nov:  Just do an enormous run (until I stop it), and never snapsnot.
	(checkpoint-bunches
	  :bunch-size most-positive-fixnum	; Never checkpoint per iteration, but only by wallclock time.
	  :never-snapshot t
	  :start-new-run t			; NOTE!  Starting a new run.
	  :snapshot-pathname-prefix "FEP0:>Snap>"	; NOTE!  Wrong place, but place must exist.  We'll never snapshot to here anyway.
	  :log-name-prefix "eyehand-cinmsf-swrus-cinih-asn-big-run-again"
	  )
	))))

(defun LOAD-BIG-RUN ()
  (wait-for-and-summarize-gc)
  (just-init)					; Must come _before_ we load the big snapshot, so that array limits are set right.
  (wait-for-and-summarize-gc)
  (time (append-all-snapshots "FEP1:>Big-Runs>14569.ibin" :query nil))
  (wait-for-and-summarize-gc)
  (time (restore-eyehand 0))
  (wait-for-and-summarize-gc))

(defun CONTINUE-BIG-RUN ()
  (load-big-run)
  (cp:execute-command "Add Paging File" "fep1:>one.page")
  (wait-for-and-summarize-gc)
  (big-run-continuation))

;;; ---

(checkpoint-bunches
  :bunch-size 5000
  :never-snapshot t
  :start-new-run t				; NOTE!  Starting a new run.
  :snapshot-pathname-prefix "FEP0:>Snap>"	; NOTE!  Wrong place, but place must exist.  We'll never snapshot to here anyway.
  :log-name-prefix "eyehand-full-crossbar"
  )


(checkpoint-bunches
  :bunch-size 5000
  :number-of-bunches 1				; Run 5000 and then stop.
  :never-snapshot t
  :start-new-run t				; NOTE!  Starting a new run.
  :snapshot-pathname-prefix "FEP0:>Snap>"	; NOTE!  Wrong place, but place must exist.  We'll never snapshot to here anyway.
  :log-name-prefix "eyehand-full-crossbar"
  )

(with-spinoff-schemas-with-recently-updated-stats-generators
  (with-ueis-history-items-all-schemas-generators
    (checkpoint-bunches
      :never-snapshot t
      :bunch-size 5000
      :number-of-bunches 1
      :start-new-run t
      :snapshot-pathname-prefix "FEP0:>Snap>"	; Irrelevant here.
      :log-name-prefix "regression-for-schema-12"
      )))

; (with-current-microworld 'eyehand::eyehand (with-action-selection (item-class-cache)))

;;; Try out goals.

(defun UNTRY-GOALS (&optional (iterations 10))
  (with-spinoff-schemas-with-recently-updated-stats-generators
    (with-ueis-history-items-all-schemas-generators
      (checkpoint-bunches
	:never-snapshot t
	:bunch-size iterations
	:number-of-bunches 1
	:start-new-run t
	:snapshot-pathname-prefix "FEP0:>Snap>"	; Irrelevant here.
	:log-name-prefix "goals"
	)
      )))

(defun TRY-GOALS-NEVER-RANDOM-OLD (&key (iterations 10)
; 				   (start 'eyehand::always-succeed)
; 				   (start 'eyehand::general-scan)
				   (start 'eyehand::general-scan-until-in-fovea)
				   (goal-stop #'stop-when-goal-achieved-arb-fn)
				   (new-run t))
  (with-spinoff-schemas-with-recently-updated-stats-generators
    (with-ueis-history-items-all-schemas-generators
      (with-starting-goal-and-generator (start)
	(checkpoint-bunches
	  :never-snapshot t
	  :bunch-size iterations
	  :number-of-bunches 1
	  :start-new-run new-run
	  :arbitrary-stop-fn goal-stop
	  :snapshot-pathname-prefix "FEP0:>Snap>"	; Irrelevant here.
	  :log-name-prefix "goals"
	  )
	))))

;;; If LOG-NAME-PREFIX is supplied, we'll snapshot and dump the end of the run.
;;; If you only want to do that when bounds exceeded, just make the number of
;;; iterations huge.
(defun TRY-GOALS-OLD (iterations &key
		      log-name-prefix
; 		      (start 'eyehand::always-succeed)
; 		      (start 'eyehand::general-scan-until-in-fovea)
		      (start 'eyehand::general-scan)
		      (goal-stop #'stop-when-goal-achieved-arb-fn)
		      (new-run t))
  (with-spinoff-schemas-with-recently-updated-stats-generators
    (with-ueis-history-items-all-schemas-generators
      (with-starting-goal-and-generator (start)
	(checkpoint-bunches
	  :never-snapshot (not log-name-prefix)
	  :bunch-size iterations
	  :number-of-bunches 1
	  :start-new-run new-run
	  :arbitrary-stop-fn goal-stop
	  :snapshot-pathname-prefix "FEP1:>Goals>"
	  :log-name-prefix (or log-name-prefix "goals")
	  )
	))))
||#

;;; My god!  I just realized that, somewhere back in the dim past, I'd evidently
;;; started doing most of my runs with WITH-UEIS-HISTORY-ITEMS-ALL-SCHEMAS-GENERATORS
;;; and apparently just copied that line ad infinitum.  I wonder how many results were
;;; invalidated?  Probably not much in goals (though obviously I should notice a performance
;;; improvement).  For a pretty reasonable run using it, see +CIN-SWRUS-CINIH-ABSPSDUCI.LINKED.1,
;;; which was linked on 1/5/94 to EYEHAND-(CHANGED-ITEM&RUS-SCHEMA)&(CHANGED-ITEM&NO-MOD).OUT.1,
;;; itself written on 9/1/93.
;;;
;;; Note that WITH-UEIS-ATTENTIVE-TO-PRIOR-TICK-GENERATORS comes pretty close to the
;;; right thing, so runs that used it are probably close to optimal.  (Its problem is using MOD-,
;;; which isn't right, and also using CINMSF, which is unnecessary.)
;;;
;;; TRY-GOALS differs from TRY-GOALS-OLD in that it uses ...UEIS-*CHANGED*-ITEMS-*DEPENDENT*...
;;; instead of ...UEIS-*HISTORY*-ITEMS-*ALL*...
;;;
;;; Well, hmm, I guess it really doesn't make that much difference.  In fact, in a run
;;; that generated ~3200-3300 schemas, ~120-160 syn items, and ~990 conjunctions,
;;; TRY-GOALS-OLD used ~128M work units, while TRY-GOALS used ~81M.  However,
;;; the old one used only ~16.5h wall-clock time, whereas the new one used ~18h.
;;; So the overhead of all the other stuff in this implementation starts to dominate,
;;; and it's actually slightly faster, for slightly higher-quality results, to use the old
;;; one.

;;; If LOG-NAME-PREFIX is supplied, we'll snapshot and dump the end of the run.
;;; If you only want to do that when bounds exceeded, just make the number of
;;; iterations huge.
(defun TRY-GOALS (iterations &key
		  log-name-prefix
; 		  (start 'eyehand::always-succeed)
; 		  (start 'eyehand::general-scan-until-in-fovea)
		  (start 'eyehand::general-scan)
		  (goal-stop #'stop-when-goal-achieved-arb-fn)
		  (new-run t)
		  (runner-fn *default-runner-fn*)
		  (snapshot-fn *default-snapshot-fn*))
  (with-spinoff-schemas-with-recently-updated-stats-generators
    (with-ueis-history-items-dependent-schemas-generators
      (with-starting-goal-and-generator (start)
	(checkpoint-bunches
	  :never-snapshot (not log-name-prefix)
	  :bunch-size iterations
	  :number-of-bunches 1
	  :start-new-run new-run
	  :arbitrary-stop-fn goal-stop
	  :snapshot-pathname-prefix "FEP1:>Goals>"
	  :log-name-prefix (or log-name-prefix "goals")
	  :runner-fn runner-fn
	  :snapshot-fn snapshot-fn
	  )
	))))

;;;; Various scorecard collectors.

(defun SCORECARD-TEST-SUITE (&key (goal-set '((eyehand::high-res-scan-into-dead-center)
					      (eyehand::general-scan-until-in-fovea-wif)))
			     (highs (list *schema-number* 1000)))
  (flet ((collect-data (h &optional (a 50) (s 100))
	   (collect-scorecards-with-lobotomies
	     :attempts a
	     :high h
	     :starts s
	     :include-all nil
	     )))
    (loop for use-goal-set in goal-set
	  for use-high in highs
	  do (let ((*scorecard-goals-to-try* use-goal-set))
	       (collect-data use-high))))
  (analyze-lobotomized-scorecards)
  (values))

(defun SC-TS-1 ()
  (loop for incremental in '(nil t)
	do (let ((*allow-incremental-replanning* incremental))
	     (scorecard-test-suite)
	     (dump-lobotomy-info)
	     (setf *all-lobotomized-scorecard-lps* nil)))
  (values))

;;; Temporary.  Just to get the very first stuff with CTXT info.
(defun SC-TS-2 ()
  (sc-ts-1)
  (flet ((collect-data (h &optional (a 50) (s 100))
	   (collect-scorecards-with-lobotomies
	     :attempts a
	     :high h
	     :starts s
	     :include-all nil
	     )))
    (loop for goal-set in '((eyehand::high-res-scan-into-dead-center)
			    )
	  for high in (list *schema-number*)
	  do (let ((*scorecard-goals-to-try* goal-set))
	       (collect-data high))))
  (analyze-lobotomized-scorecards)
  (values))

;;; Also temporary; does dumping for SC-TS-2 (since I forgot), then does some low-limit runs,
;;; with no incremental replanning.
(defun SC-TS-3 ()
  (dump-lobotomy-info)
  (setf *all-lobotomized-scorecard-lps* nil)
  (scorecard-test-suite
    :highs '(300 200))
  (analyze-lobotomized-scorecards)
  (dump-lobotomy-info)
  (values))  

(defun SC-TS-4 ()
  (append-all-snapshots "FEP1:>Macsyma-Rerun>7373.ibin"
			:load-query nil :init-query nil)
  (wait-for-gc-complete)
  (restore-eyehand 1)
  (wait-for-gc-complete)
  (sc-ts-1)
  (values))

(defun SC-TS-34 ()
  (sc-ts-3)
  (sc-ts-4))

(defun SC-TS-RELOAD ()
  (append-all-snapshots "FEP1:>Macsyma-Rerun>7373.ibin"
			:load-query nil :init-query nil)
  (wait-for-gc-complete)
  (restore-eyehand)
  (wait-for-gc-complete)
  (values))

(defun VERBOSE-SCORECARD-TEST-SUITE (&key (goal-set '((eyehand::high-res-scan-into-dead-center)
						      (eyehand::general-scan-until-in-fovea-wif)))
				     (highs (list *schema-number* 1000))
				     (err-instead-of-bashing))
  (unless (null *all-lobotomized-scorecard-lps*)
    (when err-instead-of-bashing
      (error "Sorry, ~S isn't NULL." '*all-lobotomized-scorecard-lps*))
    (setf *all-lobotomized-scorecard-lps* nil))
  (flet ((collect-data (h &optional (a 50) (s 100))
	   (collect-scorecards-with-lobotomies
	     :attempts a
	     :high h
	     :starts s
	     :include-all nil
	     )))
    (loop for use-goal-set in goal-set
	  for use-high in highs
	  do (let ((*scorecard-goals-to-try* use-goal-set))
	       (bold-format t "~2&~S = ~S~&~S = ~S~2&"
			    '*scorecard-goals-to-try* *scorecard-goals-to-try*
			    '*allow-incremental-replanning* *allow-incremental-replanning*)
	       (collect-data use-high))))
  (analyze-lobotomized-scorecards)
  (dump-lobotomy-info)
  (setf *all-lobotomized-scorecard-lps* nil)
  (values))

;;; I actually only ran this for the NIL part, at least as of 2 May 02:28.
(defun SC-TS-5 ()
  (loop for incremental in '(nil t)
	do (let ((*allow-incremental-replanning* incremental))
	     (verbose-scorecard-test-suite
	       :highs '(300 200))))
  (values))

;;; Note that this defaults on incremental replanning, e.g., it's disabled.
;;; I don't feel like collecting the whole cross product here.
(defun SC-TS-COMBED ()
  (with-combed-schema-array 'eyehand::general-scan
;     (mapcar #'schema-print-name
; 	    (listarray *schema-array*))
    (verbose-scorecard-test-suite
      :goal-set '((eyehand::high-res-scan-into-dead-center)))))

;;; Here's the other half of SC-TS-5, for after SC-TS-COMBED.
(defun SC-TS-6 ()
  (loop for incremental in '(nil t)
	do (let ((*allow-incremental-replanning* incremental))
	     (verbose-scorecard-test-suite
	       :highs '(300 200))))
  (values))

||#
;;; End of file.
