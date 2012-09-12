;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Macsyma support for the metering.

(in-package :schema)

;;; This stuff is all horribly klugy!
(defun ACCUMULATE-METER (meter)
  (loop with sum = (car meter)
	for elt in (cdr meter)
	do (incf sum elt)
	collect sum))

(defun FORMAT-ACCUMULATE-METER (meter &optional (stream t))	; The metering info itself, not the name of the symbol.
  (let ((meter (reverse meter)))		; 'cause the data is pushed onto the front of the list every time.
    (format stream "~&[~D, " (car meter))
    (format-print-list stream "~D" (accumulate-meter meter))
    (format stream "]~&"))
  (values))
	     
(defun FORMAT-NONACCUMULATE-METER (meter &optional (stream t))
  (let ((meter (reverse meter)))		; 'cause the data is pushed onto the front of the list every time.
    (format stream "~&[~D, " (car meter))
    (format-print-list stream "~D" (cdr meter))
    (format stream "]~&"))
  (values))

(defun FORMAT-X-AXIS-RANGE (low high &optional (stream t))	; For the X-axis.
  (format stream "~&[~D, " low)
  (format-print-list stream "~D"
		     (loop for i from (1+ low) to high
			   collect i))
  (format stream "]~&")
  (values))

;;; Load the output of this with BATCHLOAD("FILENAME");.
(defun WRITE-X-AXIS-BATCH-FILE (&key (x-axis-length 10761) 	; From the longest run so far (computed across all runs for consistent axes...  hmm...).
				(x-axis-name "x_axis")
				(where (format nil "schema:runs;x-axis-~D.macsyma-batch" x-axis-length)))
  (with-open-file (out where :direction :output)
    (format out "~&array(~A, float, ~D);~&~
                   fillarray(~2:*~A,~&"
	    x-axis-name
	    x-axis-length)
    (format-x-axis-range 0 x-axis-length out)
    (format out ");~&"))
  (values))

;;; Utility functions.
(defun DASHES-TO-UNDERSCORES (string)
  (substitute #\_ #\- string))

(defun MACSYMA-METER-NAME (run-name-prefix rest-of-name)
  ;; RUN-NAME-PREFIX might be NIL for WRITE-ALL-BENCHMARKED-METERS-AS-BATCH-FILES.
  (dashes-to-underscores
    (format nil "~(~:[~;~:*~A_~]~A~)" run-name-prefix rest-of-name)))

(defun MACSYMA-ARRAY-NAME (run-name-prefix vector-symbol)
  (macsyma-meter-name
    run-name-prefix
    (string-trim "*" (symbol-name vector-symbol))))

(defun PRETTY-MACSYMA-METER-NAME (run-name-prefix vector-symbol)
  (format nil "~:(~A~)"
	  (substitute #\Space #\_
		      (macsyma-meter-name run-name-prefix vector-symbol))))

(defun WRITE-METERING-BATCH-FILE-INTERNAL (&key run-name-prefix
					   output-fn
					   meters
					   x-axis-length
					   where)
  ;; Writes only the meters, not the X-axis, since you might want to write out several sets
  ;; of meters (from different snapshots), but only need to write out one X-axis.
  (with-open-file (out where :direction :output)
    (loop for vector-symbol in meters
	  do (let ((vector
		     (symbol-value vector-symbol))
		   (macsyma-array-name
		     (macsyma-array-name run-name-prefix vector-symbol)))
	       (format out "~&array(~A, float, ~D);~&~
                              fillarray(~2:*~A,~&"
		       macsyma-array-name
		       x-axis-length)
	       (funcall output-fn vector out)
	       (format out ");~&"))))
  (values))

;;; Load the output of this with BATCHLOAD("FILENAME");.
(defun WRITE-METERING-BATCH-FILE (&key (run-name-prefix "attentive")
				  (meters *vector-metering-counters*)
				  (x-axis-length 10761)	; From the longest run so far (computed across all runs for consistent axes...  hmm...).
				  (where (format nil "schema:runs;~A-metering-arrays.macsyma-batch" run-name-prefix)))
  (write-metering-batch-file-internal
    :output-fn #'format-accumulate-meter
    :run-name-prefix run-name-prefix
    :meters meters
    :x-axis-length x-axis-length
    :where where))

;;; Load the output of this with BATCHLOAD("FILENAME");.
(defun WRITE-NONACCUMULATING-METERING-BATCH-FILE (&key (run-name-prefix "attentive")
						  (nonacc-warning "nonacc")
						  (meters '(*spinoff-inner-loop-counters*))
						  (x-axis-length 10761)
						  (where
						    (format nil
							    "schema:runs;~A-nonaccumulative-metering-arrays.macsyma-batch"
							    run-name-prefix)))
  (write-metering-batch-file-internal
    :output-fn #'format-nonaccumulate-meter
    :run-name-prefix (format nil "~:[~;~:*~A-~]~A" run-name-prefix nonacc-warning)
    :meters meters
    :x-axis-length x-axis-length
    :where where))

;;; Useful when you have several to write.
(defun WRITE-A-PLOTTING-COMMAND (stream x-axis-name run-name-prefix meter-partial-name pmmn &optional (others ""))
  ;; OTHERS should start with ", " if supplied
  (let ((mmn (macsyma-meter-name run-name-prefix meter-partial-name)))
    (format stream "~&graph(~A, ~A~A, false, false~:[~;~:*, ~S~]);~&~
                      nameplot(plot_~A);~&"	; The falses prevent the first/same/last stuff from being misinterpreted as an x- or y-axis title.
	    x-axis-name
	    mmn
	    others
	    pmmn
	    mmn
	    )))

;;; Useful when you just have one thing to plot.  The various string args can use underscores or dashes.
;;; Load the output of this with BATCH("FILENAME").
(defun WRITE-SINGLE-PLOTTING-COMMAND (&key (x-axis-name "x-axis")
				      run-name-prefix
				      meter-partial-name
				      pmmn
				      (others "")
				      preamble-text	; If this isn't NIL, we'll terminate it with a newline.
				      (where (format nil "schema:runs;~A-plotting-commands.macsyma-batch"
						     (if (or (null run-name-prefix)
							     (zerop (length run-name-prefix)))
							 meter-partial-name
							 run-name-prefix))))
  (setf x-axis-name        (dashes-to-underscores x-axis-name))
  (setf run-name-prefix    (dashes-to-underscores run-name-prefix))
  (setf meter-partial-name (dashes-to-underscores meter-partial-name))
  (with-open-file (stream where :direction :output)
    (when preamble-text
      (format stream "~A~%" preamble-text))
    (write-a-plotting-command stream x-axis-name run-name-prefix meter-partial-name pmmn others))
  (values))

;;; Handy when you just have a list of numbers and want to see them plotted.
;;; Load the output of this with BATCH("FILENAME").  By default, the single file
;;; that you must load will be called SCHEMA:RUNS;SEQUENCE-PLOTTING-COMMANDS.MACSYMA-BATCH.
;;; Note that it expects that the sequence needs to be reversed, because it assumes that the sequence was a meter.
(defvar *PLOT-SIMPLE-SEQUENCE*)			; Because the various plotting interfaces want a global symbol name.  Gross, but I don't wanna rewrite 'em...

(defun PLOT-SIMPLE-SEQUENCE-INTERNAL (output-fn sequence where)
  (let ((*plot-simple-sequence* (reverse sequence))	; Reversed because FORMAT-NONACCUMULATE-METER was expecting something built with PUSH.
	(length (1- (length sequence)))
	(x-axis-batch-file (format nil where "x-axis"))
	(data-batch-file   (format nil where "data")))
    (write-x-axis-batch-file
      :x-axis-name "plot_simple_sequence_x_axis"
      :x-axis-length length
      :where x-axis-batch-file)
    (write-metering-batch-file-internal
      :output-fn output-fn
      :run-name-prefix nil
      :meters '(*plot-simple-sequence*)
      :x-axis-length length
      :where data-batch-file)
    (write-single-plotting-command
      :preamble-text (format nil "batchload(~S);~%batchload(~S);"
			     x-axis-batch-file
			     data-batch-file)
      :x-axis-name "plot_simple_sequence_x_axis"
      :run-name-prefix nil
      :meter-partial-name (macsyma-array-name nil '*plot-simple-sequence*)	; I could probably just write "plot_simple_sequence" instead...
      :where (format nil where "plotting-commands")))
  (values))

(defun PLOT-SIMPLE-SEQUENCE (sequence &key
			     (where "schema:runs;sequence-~A.macsyma-batch"))	; Totally gross---this is a FORMAT string!  Fix if I have to...
  (plot-simple-sequence-internal #'format-nonaccumulate-meter sequence where))

(defun PLOT-SIMPLE-ACCUMULATED-SEQUENCE (sequence &key
					 (where "schema:runs;sequence-~A.macsyma-batch")); Totally gross---this is a FORMAT string!  Fix if I have to...
  (plot-simple-sequence-internal #'format-accumulate-meter sequence where))

(defun PLOT-SUPERIMPOSITION-PARAMETERS (counter limit)
  (cond ((zerop counter)
	 "first")
	((= counter (1- limit))
	 "same, last")
	(t
	 "same")))

;;; Load the output of this with BATCH("FILENAME").
;;; Note that this isn't _quite_ the right thing, in that it's putting the various types of schema spinoffs
;;; into the same graph.  What we _really_ want is to make one graph for the three types of context
;;; spinoffs with the three different algorithms, then the three types of results, etc...
(defun WRITE-INTRARUN-PLOTTING-COMMANDS (&key (run-name-prefix "attentive")
					 (where (format nil "schema:runs;~A-plotting-commands.macsyma-batch" run-name-prefix))
					 ;; All of the below are Macsyma's names for these (from a batchfile), not the names of the symbol,
					 ;; without the prepended RUN-NAME-PREFIX.  Dashes will be substitute to underscores.
					 (x-axis-name "x-axis")
					 (meters-on-same-plot
					   '("spinoff-context-counters"
					     "spinoff-result-conj-counters"
					     "spinoff-result-counters"
					     ))
					 (meters-on-different-plots
					   '("spinoff-inner-loop-counters"
					     "nonacc-spinoff-inner-loop-counters"
					     )))
  ;; Assumes that you've already read in the x-axis and the various meters, etc.
  (assert (< (length meters-on-same-plot) 9))	; For PLOT-SYMBOL-TYPE below.
  (setf x-axis-name (dashes-to-underscores x-axis-name))
  (with-open-file (out where :direction :output)
    (loop with number-of-meters-on-same-plot = (length meters-on-same-plot)
	  for meter-partial-name in meters-on-same-plot
	  for counter from 0
	  for plot-symbol-type = (format nil "98~D0" (1+ counter))	; Better be 9 or less of them!
	  do (write-a-plotting-command
	       out x-axis-name run-name-prefix meter-partial-name
	       (pretty-macsyma-meter-name run-name-prefix meter-partial-name)
	       (format nil ", [~A], ~A"
		       plot-symbol-type
		       (plot-superimposition-parameters counter number-of-meters-on-same-plot))))
    (loop for meter-partial-name in meters-on-different-plots
	  do (write-a-plotting-command
	       out x-axis-name run-name-prefix meter-partial-name
	       (pretty-macsyma-meter-name run-name-prefix meter-partial-name))))
  (values))

;;; Load the output of this with BATCH("FILENAME").
;;; This plots each meter (context, result, result-conj, etc) on a separate plot.
;;; Each plot contains metering data from all n runs.
(defun WRITE-INTERRUN-PLOTTING-COMMANDS (&key (run-name-prefixes '("attentive" "changed-items" "full-crossbar"))
					 (where "schema:runs;interrun-plotting-commands.macsyma-batch")
					 (x-axis-name "x-axis")
					 (meter-names
					   '("spinoff-context-counters"
					     "spinoff-result-conj-counters"
					     "spinoff-result-counters"
					     "spinoff-inner-loop-counters"
					     )))
  (assert (< (length meter-names) 9))		; For PLOT-SYMBOL-TYPE below.
  (setf x-axis-name (dashes-to-underscores x-axis-name))
  (with-open-file (out where :direction :output)
    (loop for meter-partial-name in meter-names
	  do (loop with number-of-runs = (length run-name-prefixes)
		   for run-name-prefix in run-name-prefixes
		   for counter from 0
		   for plot-symbol-type = (format nil "98~D0" (1+ counter))	; Better be 9 or less of them!
		   do (write-a-plotting-command
			out x-axis-name run-name-prefix meter-partial-name
			(when (zerop counter)
			  (format nil "~:(~A~)"
				  (substitute #\Space #\- meter-partial-name)))
			(format nil ", [~A], ~A"
				plot-symbol-type
				(plot-superimposition-parameters counter number-of-runs))))))
  (values))

;;; Load the output of this with BATCH("FILENAME").
(defun WRITE-INTERRUN-NONACCUMULATIVE-PLOTTING-COMMANDS (&key (run-name-prefixes '("attentive" "changed-items" "full-crossbar"))
							 (nonacc-warning "nonacc")
							 (where "schema:runs;interrun-nonaccumulative-plotting-commands.macsyma-batch")
							 (x-axis-name "x-axis")
							 (meter-names
							   '("spinoff-inner-loop-counters"
							     )))
  (setf x-axis-name (dashes-to-underscores x-axis-name))
  (let ((new-prefixes
	  (loop for prefix in run-name-prefixes
		collect (format nil "~:[~;~:*~A-~]~A" nonacc-warning prefix))))
    (with-open-file (out where :direction :output)
      (loop for meter-partial-name in meter-names
	    do (loop for new-prefix in new-prefixes
		     for old-prefix in run-name-prefixes	; For labelling only.
		     do (write-a-plotting-command
			  out x-axis-name new-prefix meter-partial-name
			  (format nil "~:(~A~)"
				  (substitute #\Space #\- old-prefix)))))))
  (values))

;;; +++ Expanding a particular meter.

;;; [This one isn't too useful, since I'm expanding the nonacc cases.  Also, it would
;;; really have to sum up the list before START to yield accurate results---easy,
;;; but not yet implemented.]
(defun PARTIAL-FORMAT-ACCUMULATE-METER (meter start end &optional (stream t))	; The metering info itself, not the name of the symbol.
  (let ((meter (reverse (subseq meter start end))))	; 'cause the data is pushed onto the front of the list every time.
    (format stream "~&[~D, " (car meter))
    (format-print-list stream "~D" (accumulate-meter meter))
    (format stream "]~&"))
  (values))
	     
(defun PARTIAL-FORMAT-NONACCUMULATE-METER (meter start end &optional (stream t))
  (let ((meter (reverse (subseq meter start end))))	; 'cause the data is pushed onto the front of the list every time.
    (format stream "~&[~D, " (car meter))
    (format-print-list stream "~D" meter)
    (format stream "]~&"))
  (values))

;;; Load the output of this with BATCHLOAD("FILENAME");.
(defun WRITE-EXPANDED-METERING-BATCH-FILE (&key (meters '(*attentive-spinoff-inner-loop-counters*))
					   x-axis-start
					   x-axis-end
					   (where (format nil "schema:runs;expanded-~D-~D-metering-arrays.macsyma-batch"
							  x-axis-start
							  x-axis-end)))
  (assert (numberp x-axis-start))
  (assert (numberp x-axis-end))
  (let ((x-axis-length (- x-axis-end x-axis-start)))
    (write-metering-batch-file-internal
      :output-fn #'(lambda (meter stream)	; 'cause ...-INTERNAL wasn't written to take args for OUTPUT-FN, and I don't have time to change it now.
		     (partial-format-nonaccumulate-meter
		       meter x-axis-start x-axis-end stream))
      :run-name-prefix "partial-nonacc"
      :meters meters
      :x-axis-length x-axis-length
      :where where)
    (write-x-axis-batch-file
      :x-axis-length x-axis-length
      :x-axis-name "partial_x_axis"))
  (values))    

;;; +++ "Dumping" and "loading" the printed representation of metering info,
;;; +++ for those occasions when I want quicker processing of it for something.

(defun FAST-READ-SNAPSHOTTED-METERING-INFO (pathname prefix)
  ;; Useful for quickly reading in the printed representation of SNAPSHOT-METERING-INFO
  ;; without having to actually read the entire snapshot.  Depends on my having actually
  ;; written that out, of course, since it's not automatic with a snapshot.  PREFIX is used
  ;; to make new symbols from the symbols read in, to avoid bashing a current set of meters.
  ;; It's NOT optional, to force me to consider this issue, though the empty string works fine
  ;; if I don't care about bashing the existing meters.
  (with-open-file (in pathname)
    (let ((data (read in)))
      (loop for (old-symbol value) in data
	    for new-symbol-name = (make-prefix-for-symbol old-symbol prefix)
	    for new-symbol = (intern new-symbol-name)
	    do (format t "~&Setting ~S to a list of ~D element~:P.~&"
		       new-symbol
		       (length value))
	       (set new-symbol value))))
  (values))

;;; Special-purpose to the particular meters I wrote out in early July from here to the end of the page.
(defun READ-IN-BENCHMARKED-METERS ()
  (fast-read-snapshotted-metering-info "schema:runs;eyehand-attentive-changes.meters" "attentive")
  (fast-read-snapshotted-metering-info "schema:runs;eyehand-changed-items-all-schemas.meters" "changed-items")
  (fast-read-snapshotted-metering-info "schema:runs;eyehand-full-crossbar.meters" "full-crossbar")
  (values))

(defun WRITE-ALL-BENCHMARKED-METERS-AS-BATCH-FILES ()
  (loop with accumulative-meters = '(*spinoff-context-counters*
				      *spinoff-result-conj-counters*
				      *spinoff-result-counters*
				      *spinoff-inner-loop-counters*)  
	with nonaccumulative-meters = '(*spinoff-inner-loop-counters*)
	for run in '("attentive" "changed-items" "full-crossbar")
	;; We have to compute this ourselves, and then supply a :RUN-NAME-PREFIX of NIL.
	;; The deal here is that we must compute the meter names ourselves (so we get the right
	;; ones, which are NOT the default ones in the machine after a run---the ones we want have
	;; the run-type embedded in them as a prefix).  But the batch-file-writers also add the
	;; RUN-NAME-PREFIX to the name of the Macsyma symbols they emit, whereas we've got the
	;; right names already. Hence, we force it to NIL so things work out correctly.  This whole
	;; problem is because here we're kinda abusing the mechanism with funny meter names that
	;; aren't the normal meter names.  This is also why we have to explicitly specify :WHERE.
	for new-accumulative-meters = (loop for meter in accumulative-meters
					    collect (intern (make-prefix-for-symbol meter run)))
	for new-nonaccumulative-meters = (loop for meter in nonaccumulative-meters
					    collect (intern (make-prefix-for-symbol meter run)))
	do (write-metering-batch-file
	     :run-name-prefix nil
	     :meters new-accumulative-meters
	     :where (format nil "schema:runs;~A-metering-arrays.macsyma-batch" run))
	   (write-nonaccumulating-metering-batch-file
	     :run-name-prefix nil
	     :meters new-nonaccumulative-meters
	     :where (format nil "schema:runs;~A-nonaccumulative-metering-arrays.macsyma-batch" run)))
  (values))

;;; End of file.
