;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Various random analysis stuff.

(in-package :schema)

(defun CONTEXT-SPINOFF-PERIODICITY (&optional (meter *spinoff-context-counters*)); The original (pushed) meter, which we'll reverse before proceeding.
  (declare (special *all-schemas-every-n*))
  (loop for elt in (reverse meter)
	for i from 1				; Not zero, because of exactly when the clock ticks vs when the spinoffs happen.
	when (and (zerop (mod i *all-schemas-every-n*))
		  (not (zerop elt)))
	  collect (list i elt) into on-period
	when (and (not (zerop (mod i *all-schemas-every-n*)))
		  (not (zerop elt)))
	  collect (list i elt) into off-period
	finally
	  (return (values on-period off-period))))

(defun SHOW-SCHEMAS (&key (start 0) end vector terse)
  (setf end (if vector
		(length vector)
		*schema-number*))
  (loop for i from start below end
	for schema = (if vector
			 (aref vector i)
			 (get-schema i))
	do (format t "~&~2D ~A~&"
		   i
		   (if terse
		       (schema-print-name schema)
		       schema)))
  (values))

(defun SHOW-SCHEMAS-LIST (list &key
			  (start 0)
			  (end (length list))
			  (stream t)
			  (number-lines t)
			  terse)
  (loop for i from start below end
	for elt in (nthcdr start list)
	do (format stream "~&~:[~*~;~2D ~]~A~&"
		   number-lines
		   i
		   (if terse
		       (schema-print-name elt)
		       elt)))
  (values))

;;;; Showing tables.

(defparameter *DEFAULT-TABLE-WIDTH* 140)

(defun DEFAULT-TABLE-WIDTH ()
  #-Genera *default-table-width*		; Since I have no idea how to get this under MCL or anything else at the moment.
  #+Genera (floor (scl:send *standard-output* :inside-width)
		  (scl:send *standard-output* :character-width #\Space)))	; This correctly gets the current default sheet character style.

;;; This sorts its output across; what I really want is output sorted down instead.
(defun SHOW-THINGS-ACROSS (start end thing-vector thing-print-fn &optional (width (default-table-width)))
  (let* ((longest				; If START (but not END) is such that we don't print 'em all, this nonetheless includes them in the tally.  Deal.
	   (longest-printed-rep-vector thing-vector thing-print-fn end))
	 (columns				; Includes other formatting besides just the name, of course.
	   (floor (/ width (+ longest 5)))))
    (terpri)
    (loop for i from start below end
	  for counter from 1
	  for name = (funcall thing-print-fn (aref thing-vector i))
	  do (format t "~3D ~A~V@T"		; !SF!
		     i
		     name
		     (1+ (- longest (length name))))
	  when (zerop (mod counter columns))
	    do (terpri))
    (terpri)
    (values)))

(defun SHOW-ITEMS-ACROSS (&optional (start 0) (end *item-number*) (width (default-table-width)))
  (show-things-across start end *item-array* #'item-print-name width))

;;; This sorts its output down.  It turns out that it calculates the shape in a rather peculiar way
;;; (see TEST-SHOW-THINGS-DOWN for an example), but I actually rather like this accidental behavior.
(defun SHOW-THINGS-DOWN (start end thing-vector thing-print-fn &optional (width (default-table-width)))
  (let* ((longest				; If START (but not END) is such that we don't print 'em all, this nonetheless includes them in the tally.  Deal.
	   (longest-printed-rep-vector thing-vector thing-print-fn end))
	 (columns				; Includes other formatting besides just the name, of course.
	   (floor (/ width (+ longest 5))))
	 (rows
	   (ceiling (- end start) columns)))
    (terpri)
    (loop for row from 0 below rows
	  do (loop for column from 0 below columns
		   for i = (+ (* column rows) row start)
		   while (< i end)
		   for name = (funcall thing-print-fn (aref thing-vector i))
		   do (format t "~3D ~A~V@T"	; !SF!
			      i
			      name
			      (1+ (- longest (length name)))))
	     (terpri)))
  (values))

(defun SHOW-ITEMS-DOWN (&optional (start 0) (end *item-number*) (width (default-table-width)))
  (show-things-down start end *item-array* #'item-print-name width))

(defun SHOW-ITEMS (&optional (start 0) (end *item-number*) (width (default-table-width)))    ; In Genera, this could really just be an SCL:DEFF.
  (show-items-down start end width))

; (defun TEST-SHOW-THINGS-DOWN (&optional (max 140))
;   (let ((vector (make-array max :initial-element "Foobar")))
;     (loop for i from 0 below max
;         do (show-things-down 0 i vector #'identity)
;            (format t "~&-----")))
;   (values))

;;;; Showing what actions happened during the run.

(defun ACTION-INDEX->NAME (action-index)
  (action->name (get-action action-index)))

#+Genera					; SYS:FUNCTION-NAME.
(defun ACTION->NAME (action)
  (symbol-name (sys:function-name action)))

#+Genera
(defun SHOW-ACTIONS-THIS-RUN ()			; Genera-specific because of SYS:FUNCTION-NAME.  Surely there's a CLtL equivalent?  I can't find it...
  (let* ((length (length *actions-this-run*))
	 (action-vector (make-array length
				    :initial-contents (reverse *actions-this-run*))))
    (show-things-down 0 length action-vector #'action-index->name)))

(defun TALLY-ACTIONS-THIS-RUN ()
  (loop for action-index from 0 below *action-number*
	for action-name = (action-index->name action-index)
	do (safe-format t "~&~A~10T~D~&"
			action-name
			(count action-name *actions-this-run* :key #'action-index->name :test #'string-equal)))
  (values))

(defun ITERATIONS-TAKING-ACTION (action-name)
  (format t "~&")				; Not (TERPRI), in case we're already starting at the left margin.
  (loop for action in (reverse *actions-this-run*)
	for counter from 0
	when (string-equal action-name (action-index->name action))
	  do (format t "~D " counter))
  (format t "~&")				; Not (TERPRI), in case we wind up printing nothing.
  (values))

;;;; Getting a visual handle on how items change over time.

(def-debugging-switch SHOW-ITEMS-ANIMATION nil)

;;; This does _not_ get stored like a meter, because, at around 430 characters per
;;; iteration, it would about _double_ the space required to save a snapshot.
(def-run-variable *FORMATTED-ITEMS-STRING* nil)

(defun SHOW-ITEMS-INTO-STRING ()
  (with-output-to-string (*output-stream*)
    (microworld-status-report)))

(defun STORE-SHOW-ITEMS ()			; This should get called one per iteration.
  (when *show-items-animation-enabled*
    (with-show-items-enabled			; No matter how the variable was set, if we're doing this, we want some damned output...
      (push (show-items-into-string) *formatted-items-string*)))
  (values))

;;; Returns a string with the same characters in it as NEW.  However, each character
;;; in NEW that didn't correspond to a character in OLD is boldfaced.
#+Genera					; Character styles.
(defun HIGHLIGHT-MISMATCHES (old new &key (predicate #'char-equal) (style '(nil :bold nil)))
  (assert (= (length old) (length new)))
  (let ((out  (make-string (length old) :element-type 'character))	; A fat string.
	(highlight (si:parse-character-style style)))
    (loop for o being the array-elements of old
	  for n being the array-elements of new
	  for index from 0
	  do (setf (aref out index)
		   (if (funcall predicate o n)
		       n
		       (scl:make-character n :style highlight))))
    out))

#+Genera
(defun WEIRD-HIGHLIGHT-MISMATCHES (old new &key (predicate #'char-equal) (style '(nil :bold nil))
				   (substitutions '("*." "XO")))
  ;; Because bold (or whatever) asterisks and periods aren't the easiest things in
  ;; the world to see...  too bad I can't make 'em flash or something (now _there_
  ;; would be a great font---anything written in it _flashes_!).
  (setf style (si:parse-character-style style))
  (let ((mismatches (highlight-mismatches old new :predicate predicate :style style)))
    (loop for old being the array-elements of (first  substitutions)
	  for new being the array-elements of (second substitutions)
	  for old-styled = (scl:make-character old :style style)
	  for new-styled = (scl:make-character new :style style)
	  do (nsubstitute new-styled old-styled mismatches :test #'char=))	; Must be #'CHAR= so we only hit the styled ones...
    mismatches))

;;; A primitive one.
(defun REPLAY-SHOW-ITEMS-SIMPLE (&key (start 0)
				 (end (length *formatted-items-string*))
				 (style '(nil :bold nil)))
  (loop with old-string = nil
	for string in (subseq (reverse *formatted-items-string*) start end)
	for action in (subseq (reverse *actions-this-run*) start end)
	for index from start
	do (format t "~&~5D  ~A~:[~;  (unchanged)~]~2&~A~2&"
		   index
		   (action-index->name action)
		   (and old-string (string-equal old-string string))
		   (if old-string
		       (weird-highlight-mismatches old-string string :style style)
		       string))
	   (setf old-string string))
  (values))

;;; This one should be run on a window on which it is safe to do a clear (or clear-output-history?)
;;; It should eventually provide support for going forwards or backwards in the history, but doesn't quite yet.
(defun REPLAY-SHOW-ITEMS-ANIMATION (&optional (start 0) (end (length *formatted-items-string*)))
  (loop for string in (subseq (reverse *formatted-items-string*) start end)
	for action in (subseq (reverse *actions-this-run*) start end)
	do (format t "~:|~A~2&~A"
		   (action-index->name action)
		   string)
	   (read-char))
  (values))

;;;; Fun with snapshot summaries.

(defun SUMMARIZE-SNAPSHOTS-WITH-METER (&key (from 0)
				       (name '*spinoff-inner-loop-counters*)
				       (end (number-of-snapshotted-world-states))
				       (stream t)
				       (heading t))
  (summarize-snapshots
    :from from
    :end end
    :stream stream
    :heading heading
    :verbose t
    :extra-fn #'report-single-vector-meter
    :extra-fn-args `(,name)))

;;;; Sorting lots of runs in various axes.

;;; The basic problem here is that I've got at least a 4D dataset.  *sigh*

(defparameter *DATASET*
	      '(
		(ain	asn		ain	asn		1794	1075	49	533	532	---)	; 9 Sep 93.
		(ainmsf	asn		ain	asn		1756	993	52	533	533	---)	; 5 Dec 93.

		(ain	asn		ain	abspsduciih	1144	588	 7	398	 96	---)
		(ain	asn		ain	abspsduci	1110	518	 2	391	 55	---)
		(ain	asn		cinih	abspsduciih	 935	371	 4	348	 17	---)
		(ain	asn		cinih	abspsduci	1135	403	 4	398	 12	---)

		(cinmsf	abspsduci	ain	abspsduciih	1142	590	 7	 11	 94	---)
		(cinmsf	abspsduci	ain	abspsduci	1110	506	 3	 10	 54	---)
		(cinmsf	abspsduci	ain	asn		1622	924	33	 15	510	2)
		(cinmsf	abspsduci	cinih	abspsduciih	 935	366	 4	  8.6	 17	---)
		(cinmsf	abspsduci	cinih	abspsduci	1136	399	 4	  9.8	 12	---)
		(cinmsf	abspsduci	cinih	asn		1366	643	22	 13	 64	4)

		(cinmsf	asn		ain	abspsduciih	1145	595	 8	 33	 95	---)
		(cinmsf	asn		ain	abspsduci	1110	506	 3	 33	 54	---)
		(cinmsf	asn		cinih	abspsduciih	 935	366	 4	 29	 17	---)
		(cinmsf	asn		cinih	abspsduci	1136	399	 4	 34	 12	---)

		(cinmsf	mabspsduci	ain	asn		1650	932	38	 16	515	1)
		(cinmsf	mabspsduci	cinih	asn		1366	644	22	 14	 64	3)

		(cinmsf	swrus		ain	abspsduciih	1077	544	 6	  1.3	 90	---)
		(cinmsf	swrus		ain	abspsduci	1102	498	 2	  1.2	 53	---)
		(cinmsf	swrus		ain	asn		1395	791	11	  2.5	463	full-crossbar-rus-to-5000)
		(cinmsf	swrus		cinih	abspsduciih	 919	381	 3	  1.1	 17	fixed-hids-new-stats)
		(cinmsf	swrus		cinih	abspsduci	1134	398	 4	  1.2	 12	0)
		(cinmsf	swrus		cinih	asn		1353	688	20	  2.5	 64	fixed-moving-hais-rus-stats)

		(ain	asn		ain	nvpivs		1766	990	53	535	516	---)
		(cinmsf	asn		ain	nvpivs		1689	944	46	 44	505	---)
		(cinmsf	swrus		ain	nvpivs		1396	788	12	  2.4	447	---)
		(cinmsf	swrus		cinih	nvpivs		1348	651	20	  2.5	 62	---)

		(cinmsf	asn		ain	asn		1693	948	45	 44	524	---)
		))

;;; Don't change this without changing SORT-DATASET!
(defun SORT-DATASET-DERIVE-DATA (dataset)
  ;; Generate the extra columns in the table from the raw data.
  (loop for (spin-items spin-schemas stat-items stat-schemas schemas-total schemas-reliable
			conjs inner-spin inner-stat label) in dataset
	collect (list spin-items spin-schemas stat-items stat-schemas schemas-total schemas-reliable
		      (/ schemas-total schemas-reliable)
		      conjs inner-spin inner-stat (+ inner-spin inner-stat)
		      (float (/ schemas-reliable inner-spin))
		      (float (/ schemas-reliable inner-stat))
		      (float (/ schemas-reliable (+ inner-spin inner-stat)))
		      (float (/ schemas-total inner-spin))
		      (float (/ schemas-total inner-stat))
		      (float (/ schemas-total (+ inner-spin inner-stat)))
		      label)))

;;; This now DEPENDS on what's emitted by SORT-DATASET-DERIVE-DATA, but doesn't call it
;;; itself, because SORT-DATASET-ALL-WAYS needs to call it first (to see how many columns
;;; we'll have in all), so we shouldn't bother regenerating the data.  The basic problem here
;;; is that all three functions really depend on each other.  *sigh*
;;; PAD-VARIABLE-FORMAT should set set to a single character or a one-character string, such
;;; as "&", for use in importing the table in FrameMaker (with correct decimal alignment obtained
;;; by right-aligning everything while in a monospace font, then replacing ampersands with spaces).
(defun SORT-DATASET (dataset &key (axis-number 0) (stream t) show-snapshot-label
		     pad-variable-format round-variable-format)
  (let* ((predicate				; I suppose I could have also defined a STRING-OR-NUMBER-LESSP sort of thing...
	   (if (numberp (nth axis-number (car dataset)))
	       #'<
	       #'string-lessp))
	 (sorted
	   (stable-sort (copy-list dataset)	; Stable so any reasonable presorting is mostly preserved.
			predicate
			:key #'(lambda (entry)
				 (nth axis-number entry)))))
    (format stream "~&   Spinoffs            Stats             Schemas                 Inner loops      Reliable schemas over      ~
                      Total schemas over~:[~;    Snap~]~&~
                      Items  Schemas     Items  Schema      Total  Rel  T/R   Cj    Spin  Stat   Both     Spin  Stats   Both      ~
                      Spin  Stats    Both~&"
	    show-snapshot-label)
    (loop for (spin-items spin-schemas stat-items stat-schemas schemas-total schemas-reliable
	       total-over-reliable
	       conjs inner-spin inner-stat inner-both
	       reliable-over-spin reliable-over-stat reliable-over-both
	       total-over-spin total-over-stat total-over-both
	       label) in sorted
	  do (when round-variable-format
	       (setf inner-spin (round inner-spin))
	       (setf inner-both (round inner-both)))
	     (format stream "~&~6A ~11A ~6A ~11A  ~4,' D ~4,' D  ~$  ~2,' D    ~:[~1,1,5$~*~;~3D~:[  ~;~:*~A~:*~A~]~]  ~
                               ~3D  ~:[~1,1,6$~*~;~4D~:[  ~;~:*~A~:*~A~]~]   ~
                               ~1,1,5$  ~1,1,5$  ~1,1,5$      ~1,1,5$  ~1,1,5$  ~1,1,5$~:[~;     ~A~]~&"                               
		     spin-items spin-schemas stat-items stat-schemas schemas-total schemas-reliable
		     total-over-reliable
		     conjs
		     (integerp inner-spin) inner-spin pad-variable-format
		     inner-stat 
		     (integerp inner-both) inner-both pad-variable-format
		     reliable-over-spin reliable-over-stat reliable-over-both
		     total-over-spin total-over-stat total-over-both
		     show-snapshot-label label)))
  (values))

(defun SORT-DATASET-ALL-WAYS (&key (dataset *dataset*) (stream t) show-snapshot-label
			      pad-variable-format round-variable-format)
  (let* ((new-dataset (sort-dataset-derive-data dataset))
	 (axes (1- (length (car new-dataset)))))	; Last one is just random text, so don't bother sorting by it.
    (loop for axis from 0 below axes
	  do (format stream "~2&~'bAxis ~D:~~2&" axis)
	     (sort-dataset
	       new-dataset
	       :axis-number axis
	       :stream stream
	       :show-snapshot-label show-snapshot-label
	       :pad-variable-format pad-variable-format
	       :round-variable-format round-variable-format)))
  (values))

;;;; Dealing with the stats I created by hand, stored in *THE-STATS*.

; (defvar *THE-STATS*)
; 
; ;;; This has the pathname placed slightly differently from the automatic stuff above
; ;;; (which was, alas, written later, or this, plus all the by-hand stuff, would not have
; ;;; had to be done).
; (defun MASSAGE-STATS (&optional (stats *the-stats*))
;   (loop for (path
; 	      sig ssg uig usg
; 	      csihs-var csihs
; 	      ssuwnt-var ssuwnt
; 	      vocsihs-var vocsihs
; 	      data) in stats
; 	do (ignore csihs-var ssuwnt-var vocsihs-var)
; 	collect (list (list path
; 			    sig ssg uig usg
; 			    csihs ssuwnt vocsihs)
; 		      (loop for (i ts rs conj item prim syn spin stat)
; 			    on data
; 			    by #'(lambda (stuff)
; 				   (nthcdr 9 stuff))
; 			    collect (list i ts rs conj item prim syn spin stat)))))

;;;; Yanking statistics out of the logfiles.

;;; This gives me medium-quality statistics, e.g., only particular things from the total
;;; run.  However, since it comes from the logfiles, not the snapshots, it means it's
;;; _extremely_ quick to generate!

;;; If COMMAS-IN-USE is non-NIL, two numbers immediately surrounding a comma
;;; (with no intervening whitespace or anything else) are deemed to be one number
;;; with a comma separator.
(defun YANK-NUMBERS-FROM-LINE-AS-STRINGS (line &optional commas-in-use (start-at 0))
  (loop with length = (length line)
	for outer from start-at below length
	when (digit-char-p (aref line outer))
	  collect
 	    (coerce
	      (loop for inner from outer below length
		    for inner-char = (aref line inner)
		    while (or (digit-char-p inner-char)
			      (and commas-in-use
				   (char-equal inner-char #\,)))
		    unless (char-equal inner-char #\,)
		      collect inner-char
		    do (incf outer))
	      'string)))

(defun YANK-NUMBERS-FROM-LINE (line &optional commas-in-use (start-at 0))
  (mapcar #'parse-integer
	  (yank-numbers-from-line-as-strings line commas-in-use start-at)))

(defun MAYBE-LOGFILE-GENERATOR-ID-LINE (line)
  (let ((position (search " is " line)))
    (when position
      (subseq line (+ position 4)))))

(defun MAYBE-LOGFILE-PARAMETER-ID-LINE (line)
  (let ((position (search "=" line)))
    (when position
      (let* ((numbers (yank-numbers-from-line line nil (+ position 2)))
	     (how-many (length numbers)))
	(cond ((= how-many 0)
	       (subseq line (+ position 2)))	; Non-numeric after all (e.g., "RANDOM", etc).  Won't work if we have a param with a number in it!
	      (t
	       ;; So much for error checking.  Some perfectly valid parameter lines actually
	       ;; have more than one number on them, because they're of the form
	       ;; "*FOO* = 1000 (but its init-form would set it to 1)".  So just take the first one.
	       (car numbers)))))))

(defun MAYBE-LOGFILE-DATA-CLUMP-LINE (line)
  (cond ((search "     " line)
	 (yank-numbers-from-line line t))
	(t
	 (let ((position (search "Iteration " line)))	; Save the search for efficiency's sake.
	   (when position
	     (yank-numbers-from-line line t position))))))

(defun PROCESS-LOGFILE-LINE (line)
  (or (maybe-logfile-generator-id-line line)
      (maybe-logfile-parameter-id-line line)
      (maybe-logfile-data-clump-line   line)))

(defun PROCESS-LOGFILE-LINES-RAW (pathname)
  (with-open-file (input pathname)
    (loop for line = (read-line input nil :foo)
	  until (eq line :foo)
	  for results = (process-logfile-line line)
	  when results
	    collect (process-logfile-line line))))

(defun MASSAGE-PROCESSED-LOGFILE-LINES-1 (data)
  (list (loop for item in data
	      until (listp item)
	      collect item)
	(loop for item in data
	      when (listp item)
		collect item)))

(defun MASSAGE-PROCESSED-LOGFILE-LINES-2 (data)
  (list (car data)
	(loop for (one two three four) on (second data) by #'cddddr
	      collect (append one two three four))))

(defun PROCESS-LOGFILE-LINES (pathname)
  (let ((raw (process-logfile-lines-raw pathname)))
    (massage-processed-logfile-lines-2
      (massage-processed-logfile-lines-1 raw))))

;;; The date is the epoch at which we started recording UEIS data, before which the
;;; collection stuff above will screw up (because it's expecting more data than is present).
(defparameter *LOGFILE-UEIS-EPOCH* "07/19/93 01:00")

#+Genera
(defun PROCESS-ALL-LOGFILE-LINES (wild-spec &key (after-date *logfile-ueis-epoch*)
				  (reverse-chronological t))	; Not general, but the most often useful one.
  (flet ((path->date (path)
	   (zl:get path :creation-date)))
    (let* ((parsed-after-date
	     (when after-date
	       (time:parse-universal-time after-date)))
	   (paths
	     (cdr (fs:directory-list wild-spec :sorted)))
	   (filtered-paths
	     (if parsed-after-date
		 (remove-if-not #'(lambda (date)
				    (> date parsed-after-date))
				paths
				:key #'path->date)
		 paths))
	   (sorted-paths
	     (if reverse-chronological
		 (sort filtered-paths #'>	; We just consed it, so we can destroy it.
		       :key #'(lambda (path)
				(zl:get path :creation-date)))
		 filtered-paths)))
      (loop for (path) in sorted-paths
	    collect (list path
			  (process-logfile-lines path))))))

;;;; Finding the slope of various lines, using yanked logfile data.

(defvar *CACHED-LOGFILE-STAT-DATA* nil)

(defun CACHE-LOGFILE-STAT-DATA (&key (paths "schema:runs;*.out.newest")
				(after-date *logfile-ueis-epoch*)
				(reverse-chronological t))	; Not general, but the most often useful one.
  (unless *cached-logfile-stat-data*
    (setf *cached-logfile-stat-data*
	  (process-all-logfile-lines
	    paths
	    :after-date after-date
	    :reverse-chronological reverse-chronological)))
  (values))

(defun FIND-ITERATION-DATA (stats at-iteration)
  (let ((at-iteration-data)
	(almost-iteration-data))
    (loop for data in stats
	  for iteration = (car data)
	  until (> iteration at-iteration)
	  do (cond ((= iteration at-iteration)
		    (setf at-iteration-data data))
		   ((= iteration (1- at-iteration))
		    (setf almost-iteration-data data))))
    (values at-iteration-data almost-iteration-data)))

;;; Assumes name ends with "-GENERATOR" and hence is also at least one character long.
(defun ABBREVIATE-GENERATOR-NAME (name)
  (let* ((position (search "-GENERATOR" name))
	 (partial (subseq name 0 position)))
    (coerce (append (list (aref partial 0))
		    (loop for index from 1 below (length partial)
			  when (char-equal (aref partial (1- index)) #\-)
			    collect (aref partial index)))
	    'string)))

(defun ABBREVIATED-GENERATOR-NAMES (parameters)
  (mapcar #'abbreviate-generator-name (subseq parameters 0 4)))

(defun SHOW-GENERATORS (parameters &key (stream t) bunch)
  (destructuring-bind (spin-item spin-schema stat-item stat-schema)
      (abbreviated-generator-names parameters)
    (cond (bunch				; This is a big kluge.
	   (format stream "~A-~A-~A-~A~&"
		   spin-item spin-schema stat-item stat-schema))
	  (t
	   (format stream "~10A ~10A ~10A ~13A~&"
		   spin-item spin-schema stat-item stat-schema)))))

(defun SHOW-PATHNAME-AND-GENERATORS (pathname parameters &key (path-width 65) (stream t) no-pathname)
  ;; Show the pathname.
  (cond (no-pathname
	 (format stream "~&"))
	(t
	 (let* ((whole-name #+Genera (scl:send pathname :string-for-printing)
			    #-Genera (pathname-name pathname))
		(greater (search ">" whole-name :from-end t))
		(name-only (subseq whole-name (1+ greater))))
	   (format stream "~&~V,1,1,'.A"
		   path-width
		   name-only))))
  ;; Show the most salient generator info.
  (show-generators parameters
		   :stream stream
		   :bunch no-pathname))		; This is a big kluge.

(defun SHOW-INNER-LOOPS-LOG-SCALE (pathname parameters stats &key
				   (at-iteration 5000)
				   (log-base 10)
				   (path-width 65)
				   (stream t))
  ;; Show header
  (show-pathname-and-generators pathname parameters
				:path-width path-width
				:stream stream)
  ;; Show the log values.
  (multiple-value-bind (at-iteration-data almost-iteration-data)
      (find-iteration-data stats at-iteration)
    (let ((use-this (or at-iteration-data almost-iteration-data)))
      (cond (use-this
	     (destructuring-bind (iteration ts rs conj items prim syn spin stat)
		 use-this
	       iteration ts rs conj items prim syn	; Ignored.
	       (let ((log-spin (log spin log-base))
		     (log-stat (log stat log-base))
		     (log-both (log (+ spin stat) log-base)))
		 (format stream "~1,1,5$  ~1,1,5$  ~1,1,5$"
			 log-spin log-stat log-both)))
	     (unless at-iteration-data
	       (format stream "  [~D]"
		       (car almost-iteration-data))))
	    (t
	     (format stream "  ----------"))))))

(defun SHOW-ALL-INNER-LOOPS-LOG-SCALE (&key (data *cached-logfile-stat-data*)
				       (at-iteration 5000)
				       (log-base 10)
				       (path-width 65)
				       (stream t))
  (loop for (pathname (parameters . (stats))) in data
	do (show-inner-loops-log-scale
	     pathname parameters stats
	     :at-iteration at-iteration
	     :log-base log-base
	     :path-width path-width
	     :stream stream))
  (values))

;;;; Showing just one run, log scale.

;;; Flaw in the logic:  We're looking at TOTAL work at each step, instead of INCREMENTAL work!
;;; No wonder the loglog slope doesn't look straight!  See ONE-RUN-SCHEMAS-LOG-SCALE for clues
;;; about doing this right.
(defun ONE-RUN-INNER-LOOPS-LOG-SCALE (run-nth &optional (every-n-iterations 1000) &key
				      (data *cached-logfile-stat-data*)
				      (log-base 10)
				      (stream t))
  (destructuring-bind (pathname (parameters . (stats)))
      (nth run-nth data)
    (let ((run (reverse stats))			; Reversed so we can find 5000 before 4999.
	  (exactly nil)
	  (results nil))
      (show-pathname-and-generators pathname parameters :stream stream)	; Show header.
      (flet ((push-data (iteration spin stat)	; Accumulate data in reverse order.
	       (push (list iteration spin stat) results)))
	(loop for (iteration ts rs conj items prim syn spin stat) in run
	      do (ignore ts rs conj items prim syn)
		 (let ((mod (mod iteration every-n-iterations)))
		   (cond ((zerop mod)
			  (setf exactly t)
			  (push-data iteration spin stat))
			 ((or (= mod (1- every-n-iterations))
			      (= mod 1))
			  (unless exactly	; We just got an exact one, so ignore this one.
			    (setf exactly nil)
			    (push-data iteration spin stat)))
			 (t nil)))))
      (let ((old-log-spin 0)
	    (old-log-stat 0)
	    (old-log-both 0)
	    (last-spin 0)
	    (last-stat 0)
	    (last-iteration 0)
	    (total-loglog 0))
	(loop for (current-iteration current-spin current-stat) in results    ; Now show the data in forward order.
	      for counter from 0		; So we don't show differences on the very first one.
	      do (let* ((spin (- current-spin last-spin))
			(stat (- current-stat last-stat))
			(iteration (- current-iteration last-iteration))
			(log-spin (log spin log-base))
			(log-stat (log stat log-base))
			(log-both (log (+ spin stat) log-base))
			(diff-spin (- log-spin old-log-spin))
			(diff-stat (- log-stat old-log-stat))
			(diff-both (- log-both old-log-both))
			(loglog (float (/ (log iteration 10) log-both))))
		   (setf last-spin current-spin)
		   (setf last-stat current-stat)
		   (setf last-iteration current-iteration)
		   (setf old-log-spin log-spin)
		   (setf old-log-stat log-stat)
		   (setf old-log-both log-both)
		   (incf total-loglog loglog)
		   (format stream "~&~5D ~5D ~3,1,8$ ~3,1,8$ ~3,1,8$  ~3,1,8$  ~:[~3,1,8$ ~3,1,8$ ~3,1,8$~;~]~&"
			   current-iteration	; Not ITERATION!  We want a cumulative total here.
			   iteration		; And a difference here...
			   log-spin log-stat log-both
			   loglog
			   (zerop counter)
			   diff-spin diff-stat diff-both))
	      finally
		(format stream "~&Loglog average:  ~3,1,8$~&"
			(/ total-loglog (1+ counter)))))))
  (values))

(defun ALL-RUNS-INNER-LOOPS-LOG-SCALE (&optional (limit 10000)
				       (every-n-iterations 1000)
				       &key
				       (data *cached-logfile-stat-data*)
				       (log-base 10)
				       (stream t))
  (loop for n from 0 below (min limit (length data))
	do (one-run-inner-loops-log-scale
	     n every-n-iterations
	     :data data
	     :log-base log-base
	     :stream stream))
  (values))

;;;; Schemas instead of interations.

;;; Horrible amounts of code duplication here.  I'm in a hurry.

(defun ONE-RUN-SCHEMAS-LOG-SCALE (run-nth which-schemas	; :TOTAL, :RELIABLE, or :UNRELIABLE.
				  &optional (every-n-schemas 100) &key
				  (data *cached-logfile-stat-data*)
				  (log-base 10)
				  (stream t))
  (destructuring-bind (pathname (parameters . (stats)))
      (nth run-nth data)
    (let ((run stats)				; Not reversed!
	  (old-q 0)
	  (results nil))
      (show-pathname-and-generators pathname parameters :stream stream)	; Show header.
      (flet ((push-data (iteration spin stat)	; Accumulate data in reverse order.
	       (push (list iteration spin stat) results)))
	(loop for (iteration ts rs conj items prim syn spin stat) in run
	      do (ignore iteration conj items prim syn)
		 (let* ((compare (ecase which-schemas
				   (:total ts)
				   (:reliable rs)
				   (:unreliable (- ts rs))))
			(new-q (floor compare every-n-schemas)))
		   (when (> new-q old-q)
		     (setf old-q new-q)
		     (push-data compare spin stat)))))
      (setf results (reverse results))		; Make sure we get them in ascending order!
      (let ((old-log-spin 0)
	    (old-log-stat 0)
	    (old-log-both 0)
	    (last-spin 0)
	    (last-stat 0)
	    (last-compare 0)
	    (total-loglog 0))
	(loop for (current-compare current-spin current-stat) in results	; Now show the data in forward order.
	      for counter from 0		; So we don't show differences on the very first one.
	      do (let* ((spin (- current-spin last-spin))
			(stat (- current-stat last-stat))
			(compare (- current-compare last-compare))
			(log-spin (log spin log-base))
			(log-stat (log stat log-base))
			(log-both (log (+ spin stat) log-base))
			(diff-spin (- log-spin old-log-spin))
			(diff-stat (- log-stat old-log-stat))
			(diff-both (- log-both old-log-both))
			(loglog (float (/ (log compare 10) log-both))))
		   (setf last-spin current-spin)
		   (setf last-stat current-stat)
		   (setf last-compare current-compare)
		   (setf old-log-spin log-spin)
		   (setf old-log-stat log-stat)
		   (setf old-log-both log-both)
		   (incf total-loglog loglog)
		   (format stream "~&~5D ~5D ~3,1,8$ ~3,1,8$ ~3,1,8$  ~3,1,8$  ~:[~3,1,8$ ~3,1,8$ ~3,1,8$~;~]~&"
			   current-compare	; Not COMPARE!  We want a cumulative total here.
			   compare		; And a difference here...
			   log-spin log-stat log-both
			   loglog
			   (zerop counter)
			   diff-spin diff-stat diff-both))
	      finally
		(format stream "~&Loglog average:  ~3,1,8$~&"
			(/ total-loglog (1+ counter)))))))
  (values))

(defun ALL-RUNS-SCHEMAS-LOG-SCALE (which-schemas &optional
				   (limit 10000)
				   (every-n-schemas 50)
				   &key
				   (data *cached-logfile-stat-data*)
				   (log-base 10)
				   (stream t))
  (loop for n from 0 below (min limit (length data))
	do (one-run-schemas-log-scale
	     n which-schemas
	     every-n-schemas
	     :data data
	     :log-base log-base
	     :stream stream))
  (values))

;;; This and some simple Macsyma plots show that n^2 is only straight
;;; on loglog paper, not linlog or loglin paper.  *sigh*
; (defun N-EXPT-LOG (n &optional (expt 2))
;   (loop for i from 1 to n
; 	for raised = (expt i expt)
; 	for log-i = (log i 10)
; 	for log-raised = (log raised 10)
; 	do (format t "~&~4D ~4D   ~10D   ~10D~:[   ~2,1,5$~;~]~&"
; 		   i raised
; 		   log-i log-raised
; 		   (zerop log-raised)
; 		   (unless (zerop log-raised)
; 		     (float (/ (log raised 10) (log i 10))))))
;   (values))

(defun LOG-WORK-STEP-SCHEMAS-VS-LOG-SCHEMAS (run-nth &key
					     (which-schemas :reliable)	; :TOTAL, :RELIABLE, or :UNRELIABLE.
					     (data *cached-logfile-stat-data*)
					     (stream t)
					     no-pathname)
  (destructuring-bind (pathname (parameters . (stats)))
      (nth run-nth data)
    (let ((run stats)				; Not reversed!
	  (results nil))
      (show-pathname-and-generators pathname parameters
				    :stream stream
				    :no-pathname no-pathname)	; Show header.
      (flet ((push-data (iteration spin stat)	; Accumulate data in reverse order.
	       (push (list iteration spin stat) results)))
	(loop for (iteration ts rs conj items prim syn spin stat) in run
	      do (ignore iteration conj items prim syn)
		 (let* ((compare (ecase which-schemas
				   (:total ts)
				   (:reliable rs)
				   (:unreliable (- ts rs)))))
		   (push-data compare spin stat))))
      (setf results (reverse results))		; Make sure we get them in ascending order!
      (let ((last-spin 0)
	    (last-stat 0)
	    (last-compare 0)
	    (total-compare 0))
	(loop for (current-compare current-spin current-stat) in results	; Now show the data in forward order.
	      do (let ((compare (- current-compare last-compare)))
		   (when (plusp compare)
		     ;; If we didn't generate any schemas, or if the total went DOWN (only
		     ;; possible for non-:TOTAL runs), then don't update anything until this changes.
		     ;; This means that the work counted will be all the work since the last
		     ;; generated schema to the next one, which is correct.  This works because
		     ;; CURRENT-SPIN and CURRENT-STAT are stored as CUMULATIVE totals.
		     (let ((spin (- current-spin last-spin))
			   (stat (- current-stat last-stat)))
		       (setf last-spin current-spin)
		       (setf last-stat current-stat)
		       (setf last-compare current-compare)
		       (incf total-compare compare)
		       (let* ((both (+ spin stat))
			      (work-step-schema (/ both compare)))
			 (when (minusp work-step-schema)
			   (break))
			 (format stream "~&~3,1,8$ ~3,1,8$~&"
				 (log total-compare)
				 (log work-step-schema))))))))))
  (values))

(defun ALL-LOG-WORK-STEP-SCHEMAS-VS-LOG-SCHEMAS (which-schemas &key
						 (limit 10000)
						 (data *cached-logfile-stat-data*)
						 (stream t)
						 no-pathname)
  (loop for n from 0 below (min limit (length data))
	do (log-work-step-schemas-vs-log-schemas
	     n
	     :which-schemas which-schemas
	     :data data
	     :stream stream
	     :no-pathname no-pathname))
  (values))

(defun ALL-LOG-WORK-STEP-SCHEMAS-VS-LOG-SCHEMAS-TO-FILES (which-schemas &key
							  (output-directory "MC:/u/foner/Frame/Test/Improv/New/")
							  (limit 10000)
							  (data *cached-logfile-stat-data*)
							  (no-pathname t))	; Note changed default.
  (setf output-directory (pathname output-directory))
  (loop for n from 0 below (min limit (length data))
	do (destructuring-bind (input-pathname (parameters . (stats)))
	       (nth n data)
	     parameters stats			; Ignored.
	     (let* ((old-name (pathname-name input-pathname))
		    (trimmed-name (string-trim "+" old-name))
		    (new-name (string-append
				(subseq (format nil "~A" which-schemas) 0 1)
				"-"
				trimmed-name))
		    (output-pathname
		      (make-pathname
			:host (pathname-host output-directory)
			:directory (pathname-directory output-directory)
			:name new-name
			:type "TXT")))
	       (with-open-file (stream output-pathname :direction :output)
		 (log-work-step-schemas-vs-log-schemas
		   n
		   :which-schemas which-schemas
		   :data data
		   :stream stream
		   :no-pathname no-pathname)))))
  (values))

;;; End of file.
