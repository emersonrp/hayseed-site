;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: HAMSTERDAM; -*-

;;;; Interface code between the raw linkage from Lisp to Hamsterdam and the
;;;; schema system proper.  This code translates from action numbers to actual
;;;; action requests for the socket connection, and parses the incoming sensor
;;;; echoes that go into setting sensory item bits.

(in-package :hamsterdam)

;;; Build the macros we'll need.
(def-microworld HAMSTERDAM)

;;;; General linkage stuff for HAMSTERDAM-TOPLEVEL.LISP's use of DEF-MICROWORLD-TOPLEVEL.

(defvar *HAMSTERDAM-INPUT-BUFFER* nil)
(defvar *HAMSTERDAM-OUTPUT-BUFFER* nil)

(defmacro WITH-GLOBAL-BUFFERS-AND-SOCKET (&body body)
  `(with-buffers-and-socket (*hamsterdam-input-buffer* *hamsterdam-output-buffer*)
     ,@body))

;;;; Parsing sensor echos and setting the appropriate sensory item bits.

;;; The flow of control is as follows.  Taking an action sets the output buffer to the
;;; command to be executed, and executes it.  Before the action "completes", we
;;; read the sensor echo and store it in the input buffer.  It is that input buffer that
;;; we now examine here.

;;; +++ Discretizing range information.

;;; Any raw sensor range is compared to the elements of this list, which must be in
;;; ascending order.  The range is quantized by assigning it the leftmost position in
;;; this list for which the list's value is greater than the raw range.  Any raw range
;;; greater than the largest quantity in the list is "infinitely" far away, aka out of
;;; range.  Hamsterdam actually uses 10e6 for a totally unset ray in any given update
;;; cycle (e.g., we didn't see anything), but I'm making this somewhat smaller to
;;; give more precision to ranges that we're actually likely to see inside the world.
(defparameter *DISCRETE-RANGES* '(1 10 100 1000 10000))

;;; Note that, if the range is very large, this might return NIL.
(defsubst COMPUTE-DISCRETE-RANGE (raw-range)
  (position raw-range *discrete-ranges*
	    :test #'(lambda (input elt)
		      (< input elt))))

;;; +++ Mapping Hamsterdam tags to our tags.

;;; The lookup table of Hamsterdam tags to our tags.  Determined by inspection.
;;; Duplicates on either side of the are allowed.  It's probably not worth making
;;; a hash table to speed this up, considering that it's supposed to be only half
;;; a dozen items or so.
(defparameter *SENSOR-TAG-MAPPINGS*
	      '((??? :wall)
		(??? :hamster)
		(??? :predator)
		(??? :food)
		(??? :water)))

;;; For debugging...
(defvar *ALL-RAW-TAGS-SEEN* nil)
(def-debugging-switch RECORD-TAGS t)

;;; If we get an unknown tag, this will return NIL.
(defsubst COMPUTE-TAG (raw-tag)
  (when *record-tags-enabled*
    (pushnew raw-tag *all-raw-tags-seen*))
  (second (assoc raw-tag *sensor-tag-mappings*)))

;;; +++ Getting the raw data into a couple of arrays for further processing.

(defparameter *SENSOR-FAN-RAYS* 15)
(defparameter *SENSOR-FAN-OBJECT-TAGS*   (make-array *sensor-fan-rays*))
(defparameter *SENSOR-FAN-OBJECT-RANGES* (make-array *sensor-fan-rays*))

;;; Expects something suitable for READ, of the form ((tag1 range1) ... (tagn rangen)).
;;; It turns out that the first pair is the most-counterclockwise sensor ray as it appears
;;; in the sensor fan display in Hamsterdam, and the last pair is the most-clockwise, but
;;; we actually don't care here.
(defun PARSE-INPUT-BUFFER (&optional (buffer *hamsterdam-input-buffer*))
  (let ((form (with-standard-io-environment
		(read-from-string buffer))))
    (unless (consp form)			; Must be a non-NULL list.
      (error "Got malformed sensor echo ~S from buffer~&~S"
	     form buffer)
    (unless (= (length form) *sensor-fan-rays*)	; Must be correct length.
      (error "Sensor echo ~S, of length ~D, was not of required length ~D, from buffer~&~S"
	     form (length form)
	     *sensor-fan-rays* buffer))
    (loop for (raw-tag raw-range) in form
	  for index from 0
	  for tag =   (compute-tag raw-tag)
	  for range = (compute-discrete-range raw-range)
	  do (setf (aref *sensor-fan-object-raw-tags*   index) tag)
	     (setf (aref *sensor-fan-object-raw-ranges* index) range))))
  (values))

;;; End of file.
