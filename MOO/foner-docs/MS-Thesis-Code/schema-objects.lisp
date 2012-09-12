;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; The objects and data structures defined in the schema mechanism.
;;;; This was originally the first 60% of SCHEMA.LISP.

;;; [This file was inspired by Ramstad's implementation of Drescher's
;;; system, but Ramstad's version has by now been 80% or more rewritten.]

(in-package :schema)

;;; Abbreviations used in this file:
;;; acc     = accessibility
;;; conj    = conjunction
;;; conjs   = conjunctions
;;; cons    = consistent
;;; consy   = consistency
;;; ext     = extended
;;; gd      = goal-directed
;;; lc      = local
;;; lcly    = locally
;;; neg     = negative
;;; pos     = positive
;;; stats   = statistics
;;; syn     = synthetic

;;; Evaluate in interpreter before compiling if it is desired to see
;;; a vast amount of information regarding the compilation process.
; #+Lucid (compiler-options :messages t :show-optimizations t :warnings t)

;;;; Global variables.

;;; +++ [Original Ramstad rationale for where to set various constants...]
;;; From the original implementation, 90 % of 4K processors were used
;;; to store schemas, so 3600 schemas is used as the maximum 
;;; (the other 10% were used to store goal-directed actions).
;;; The item array must be large enough to contain roughly 140 primitive
;;; items plus the synthetic items (estimated conservatively at 160)
;;; for a total of 300.
;;; Each conjunctive item must first exist as the result of a reliable
;;; schema -- max of 3600 -- estimated conservatively as 200.
;;; *-maximum* -- maximum possible index for array.
;;; *-number* -- next position to insert.
;;; ---

(defvar *CLOCK-TICK* 0)

(defvar *SCHEMA-NUMBER* 0)
(defvar *ITEM-NUMBER* 0)
(defvar *CONJ-NUMBER* 0)
(defvar *SYN-ITEM-NUMBER* 0)
(defvar *ACTION-NUMBER* 0)

(proclaim '(fixnum *clock-tick* *schema-number*
                   *item-number* *syn-item-number*
                   *conj-number* *action-number*))

;;; Utility used in a couple of places.
(defsubst PRIMITIVE-ITEM-NUMBER ()
  (- *item-number* *syn-item-number*))

(deflimit *SCHEMA-MAXIMUM* 3600 3600)

;;; If you change *CONJ-MAXIMUM* to a larger value without recompiling the whole
;;; system, you'll probably have to also call UPDATE-ALL-OBJECTS-FOR-NEW-*CONJ-MAXIMUM*
;;; in order to resize all the various random arrays that _aren't_ created at runtime that
;;; depend upon it.
(deflimit *CONJ-MAXIMUM* 996 200)		; Was 300 until changed by Schema 12.62 on 23 Apr 94.

;;; The analysis above indicates that *ITEM-MAXIMUM* should be 300, but it's
;;; inexplicably 500, even in the original code.  Since I'm not sure whether adjusting
;;; it back down to 300 is going to cause me grief if I try to reload a snapshot, leave
;;; it at 500.  Note that this was actually _too big_ for *MICROWORLD-STATE*'s
;;; definition as a 25-long array of fixna (holding 12 states each, thus 300 total items
;;; available there), but this was apparently never noticed because we never
;;; generated the (- 300 ~140) = ~160 syn-items it would take to overflow that
;;; array bound.
(deflimit *ITEM-MAXIMUM* (+ 140 160) (+ 140 160 200))

(deflimit *SYN-ITEM-MAXIMUM* 350 350)

(deflimit *ACTION-MAXIMUM* 25 25)

;;;; Flag datatype.

;;; Datatype used to indicate true or false for various things about
;;; schemas, goal-directed actions and synthetic items.
;;; Used repeatedly in all structures with compressed data.
;;; Uses fixnum math and implemented with macros; should be efficient.
;;; Not separately tested, but tested thoroughly via item and schema
;;; testing.
;;; True represented as 1, false represented as 0.
;;; (Implemented as bits to save memory.  See flag-record below.)

;;; Notes on functions:
;;; FLAG-PARSE
;;;   takes T/NIL returns true/false flag
;;; FLAG-UNPARSE
;;;   takes true/false flag returns "+"/"-"

(defconstant *FLAG-BITS* 1)
(defconstant *FLAGS-IN-FIXNUM* (floor *safe-fixnum-length* *flag-bits*))

(deflimit *FIXNA-REQUIRED-TO-HOLD-ALL-ITEM-FLAGS*
	  (ceiling *item-maximum* *flags-in-fixnum*)
  15)

(deflimit *FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-FLAGS*
	  (ceiling *conj-maximum* *flags-in-fixnum*)
  10)

(defmacro MAKE-FLAG-TRUE () `(the fixnum 1))
(defmacro MAKE-FLAG-FALSE () `(the fixnum 0))
(defmacro FLAG-EQ (x y) `(fix= ,x ,y))
(defmacro FLAG-TRUEP (x) `(flag-eq (make-flag-true) ,x))
(defmacro FLAG-FALSEP (x) `(flag-eq (make-flag-false) ,x))
(defmacro FLAG-INVERSE (x)
  `(if (flag-truep ,x)
       (make-flag-false)
       (make-flag-true)))

(defmacro FLAG-PARSE (x)
  `(if ,x
       (make-flag-true)
       (make-flag-false)))

(defmacro FLAG-UNPARSE (x)
  `(if (flag-truep ,x)
       "+"
       "-"))

;;;; Flag-record datatype.

;;; Simple way of packing up to 24 flags together into one fixnum.
;;; FLAG-RECORD-GET-INITIAL can be called to determine what the
;;; value returned by MAKE-FLAG-RECORD (a flag record which initially
;;; has all values set false) should be.

;;; Notes on functions:
;;; FLAG-RECORD-GET-INITIAL
;;;   returns the value which should be used by make-flag-record

(defmacro MAKE-FLAG-RECORD () `(the fixnum 0))

(defmacro FLAG-RECORD-GET-FLAG (flag-record position)
  `(ldb (byte 1 ,position) ,flag-record))

(defun FLAG-RECORD-GET-INITIAL ()
  (with-generalized-reference foo
    (dotimes (x *flags-in-fixnum*)
      (setf (flag-record-get-flag foo x) (make-flag-false)))))

;;;; Flag-array datatype.

;;; Fairly simple way of keeping more than 24 flags and accessing them
;;; in a transparent manner.

;;; Notes on functions:
;;; FLAG-ARRAY-GET-FLAG 
;;;   takes array and flag-index and returns flag
;;; FLAG-ARRAY-GET-FLAG-SPECIFIC
;;;   takes ARRAY, ARRAY-INDEX and RECORD-POSITION and returns flag.
;;; Note: these two accessors support different ways of getting the
;;; same information.   -SPECIFIC is faster in those cases where the two
;;; values are already computed.
;;; FLAG-ARRAY-IOR
;;;   This function does an inclusive or between the source array and
;;;   destination array and places the result in the destination array.
;;;   It is a big win in certain situations and some code has been
;;;   written to take advantage of it.
;;; FLAG-ARRAY-INCLUDED-P
;;;   Returns T if all the on flags (1's) in the slave-array are also
;;;   on in the master-array.

(defmacro MAKE-FLAG-ARRAY (max-index)
  `(make-array ,max-index
               :initial-element (make-flag-record)
               :element-type 'fixnum))

(defmacro FLAG-ARRAY-GET-FLAG (array flag-index)
  `(flag-record-get-flag (aref ,array (floor ,flag-index *flags-in-fixnum*))
			 (rem ,flag-index *flags-in-fixnum*)))

(defmacro FLAG-ARRAY-GET-FLAG-SPECIFIC (array array-index record-position)
  `(flag-record-get-flag (aref ,array ,array-index) ,record-position))

(defmacro FLAG-ARRAY-IOR (source-array dest-array max-index)
  `(dotimes (x ,max-index)
     (setf (aref ,dest-array x)
	   (logior (aref ,dest-array x)
		   (aref ,source-array x)))))

(defmacro FLAG-ARRAY-INCLUDED-P (master-array slave-array max-index)
  `(dotimes (x ,max-index t)
    (let ((slave-entry (aref ,slave-array x)))
      (if (fix/= 0 slave-entry)
          (when (fix/= slave-entry (logand (aref ,master-array x) slave-entry))
	    (return nil))))))

(defmacro FLAG-ARRAY-CLEAR (array max-index)
  `(dotimes (x ,max-index)
    (setf (aref ,array x) (make-flag-record))))

(defmacro FLAG-ARRAY-COPY (source-array dest-array max-index)
  `(dotimes (x ,max-index)
    (setf (aref ,dest-array x) (aref ,source-array x))))

(defmacro PRINT-FLAG-ARRAY-ENABLED (array max-index)
  `(dotimes (x ,max-index)
    (dotimes (y *flags-in-fixnum*)
     (format *output-stream* "~A "
             (flag-unparse
              (flag-array-get-flag-specific ,array x y))))
    (format *output-stream* "~%")))

;;;; State datatype.

;;; Used to indicate state of primitive/conjunctive/synthetic items.
;;; Can be on, off or unknown.
;;; Uses fixnum math and implemented with macros; should be efficient.
;;; Not separately tested, but tested thoroughly as used in many
;;; different functions.
;;; Note that all of these functions depend on representation.
;;; ON represented as 2, OFF as 1, UNKNOWN as 0.

;;; Notes on functions:
;;; MAKE-STATE-BOTH
;;;   This is used by the implementation as a placeholder when an item
;;;   could be either on or off, especially for some esoteric uses of
;;;   STATE-ARRAY-IOR.
;;; STATE-INVERSE
;;;   Takes ON/OFF state, returns OFF/ON.
;;;   Does NOT work for UNKNOWN states.
;;; STATE-PARSE
;;;   Takes T/NIL, returns ON/OFF state.
;;; STATE-UNPARSE
;;;   Takes ON/OFF/UNKNOWN state, returns "*"/"."/"?".

(defconstant *STATE-BITS* 2)
(defconstant *STATES-IN-FIXNUM* (floor *safe-fixnum-length* *state-bits*))

(deflimit *FIXNA-REQUIRED-TO-HOLD-ALL-ITEM-STATES*
	  (ceiling *item-maximum* *states-in-fixnum*)
  25)

(deflimit *FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-STATES*
	  (ceiling *conj-maximum* *states-in-fixnum*)
  20)

(defmacro MAKE-STATE-ON () `(the fixnum 2))

(defmacro MAKE-STATE-OFF () `(the fixnum 1))

(defmacro MAKE-STATE-UNKNOWN () `(the fixnum 0))

(defmacro MAKE-STATE-BOTH () `(the fixnum 3))

(defmacro STATE-EQ (state1 state2)
  `(fix= ,state1 ,state2))

(defmacro STATE-NOTEQ (state1 state2)
  `(fix/= ,state1 ,state2))

(defmacro STATE-ON-P (state)
  `(state-eq (make-state-on) ,state))

(defmacro STATE-OFF-P (state)
  `(state-eq (make-state-off) ,state))

(defmacro STATE-UNKNOWN-P (state)
  `(state-eq (make-state-unknown) ,state))

(defmacro STATE-INVERSE (state)
  `(lognot ,state))

(defmacro STATE-PARSE (condition)
  `(if ,condition
       (make-state-on)
       (make-state-off)))

(defun STATE-UNPARSE (state)
  (cond ((state-on-p state) "*")
        ((state-off-p state) ".")
        (t "?")))

;;; State-record datatype.

;;; Simple way of packing up to 12 states together into one fixnum.
;;; STATE-RECORD-GET-INITIAL can be called to determine what the value
;;; returned by MAKE-STATE-RECORD (a state record which initially has
;;; all states set unknown) should be.

(defmacro MAKE-STATE-RECORD () `(the fixnum 0))

(defmacro STATE-RECORD-GET-STATE (number position)
  `(ldb (byte *state-bits* (fix* ,position *state-bits*)) ,number))

;;; Currently, this is identically equal to 0.  If states changed, this might.
(defun STATE-RECORD-GET-INITIAL ()		; &&& No callers.  Hmm.
  (with-generalized-reference foo
    (dotimes (x *states-in-fixnum*)
      (setf (state-record-get-state foo x) (make-state-unknown)))))

;;;; State-array datatype.

;;; Used to keep item conjunctions within schema, in conj array
;;; and for various other purposes.
;;; Highly dependent on representation of state datatype.
;;; Represented as an array of state-records where each state-record
;;; can hold 12 states.

;;; Notes on functions:
;;; STATE-ARRAY-GET-STATE
;;;   Takes ARRAY and STATE-INDEX and returns state.
;;; STATE-ARRAY-GET-STATE-SPECIFIC
;;;   Takes ARRAY, ARRAY-INDEX and RECORD-POSITION and returns state.
;;; Note: these two accessors support different ways of getting the
;;; same information.  -SPECIFIC is faster in those cases where the two
;;; values are already computed.
;;; STATE-ARRAY-COPY-POS-FLAG
;;;   This function takes a source STATE-ARRAY and a destination
;;;   FLAG-ARRAY.   Wherever the STATE-ARRAY has an ON state, the
;;;   corresponding flag array position gets set true.
;;; STATE-ARRAY-COPY-NEG-FLAG
;;;   Similar function to STATE-ARRAY-COPY-POS-FLAG except the
;;;   destination FLAG-ARRAY has flag positions set true wherever the
;;;   source STATE-ARRAY has an OFF state.
;;; STATE-ARRAY-IOR
;;;   This function does a bitwise inclusive or between the source
;;;   array and destination array and places the result in the
;;;   destination array.
;;;   It is a big win in certain situations and some code has been
;;;   written to take advantage of it.
;;; STATE-ARRAY-INCLUDED-P
;;;   This function is used to check if a if a context/result is
;;;   satisfied.  It takes two state-arrays and indicates if the second
;;;   array is included in the first.  It assumes representation of
;;;   states as 00 is unknown, 10 is on, 01 is off.
;;;   Given the state of the microworld as a state-array s, and a
;;;   context state-array c, the context is satisfied if (and s c) = c.
;;;   On the other hand, if some context bit is not matched by the
;;;   state of the microworld, then the result of (and s c) will
;;;   differ from c in that bit position.
;;; STATE-ARRAY-GET-PRINT-NAME
;;;   Given a STATE-ARRAY, this function returns a human-readable
;;;   string which corresponds to the STATE-ARRAY if it were a
;;;   conjunction of items, where having a non-unknown state in a
;;;   given position indicates that that particular item is included
;;;   in the conj (positive if on, negative if off).

;;; State-array datatype functions.

(defmacro MAKE-STATE-ARRAY (max-index)
  `(make-array ,max-index
               :initial-element (make-state-record)
               :element-type 'fixnum))

(defmacro STATE-ARRAY-GET-STATE (array state-index)
  `(state-record-get-state
    (aref ,array (floor ,state-index *states-in-fixnum*))
    (rem ,state-index *states-in-fixnum*)))

(defmacro STATE-ARRAY-GET-STATE-SPECIFIC (array array-index record-position)
  `(state-record-get-state
    (aref ,array ,array-index)
    ,record-position))

(defsubst STATE-ARRAY-CARDINALITY (array max-index)
  (let ((count 0))
    (dotimes (x max-index)
      (dotimes (y *states-in-fixnum*)
	(unless (state-unknown-p (state-array-get-state-specific array x y))
	  (incf count))))
    count))

;;; The confusing name has a reason:  This thing actually sets a bunch of FLAGS
;;; corresponding to the input STATES.  There had better be as many flags as states!
;;;
;;; NOTE that it's _completely_ unclear to me why we bother to count them as we
;;; make the flag array!  The only caller (MAKE-CONJ) evers bother to check; this is
;;; clearly a leftover from some other incarnation that cared.  Given that it's just an
;;; incf, and only when there's actually a state in the desired state, the overhead is
;;; pretty minimal, though, so I'll leave it in.
(defmacro STATE-ARRAY-COPY-SOME-FLAG (predicate source-array dest-array max-source-fixnum-index)
  `(let ((count 0))
     (dotimes (x ,max-source-fixnum-index)
       (let ((num (aref ,source-array x)))
	 (dotimes (y *states-in-fixnum*)
	   (when (,predicate (state-record-get-state num y))
	     (setf (flag-array-get-flag ,dest-array count)
		   (make-flag-true)))
	   (setf count (fix1+ count)))))))

(defmacro STATE-ARRAY-COPY-POS-FLAG (source-array dest-array max-source-fixnum-index)
  `(state-array-copy-some-flag state-on-p  ,source-array ,dest-array ,max-source-fixnum-index))

(defmacro STATE-ARRAY-COPY-NEG-FLAG (source-array dest-array max-source-fixnum-index)
  `(state-array-copy-some-flag state-off-p ,source-array ,dest-array ,max-source-fixnum-index))

(defmacro STATE-ARRAY-IOR (source-array dest-array max-index)
  `(dotimes (x ,max-index)
     (setf (aref ,dest-array x)
	   (logior (aref ,dest-array x)
		   (aref ,source-array x)))))

(defmacro STATE-ARRAY-INCLUDED-P (master-array slave-array max-index)
  `(dotimes (x ,max-index t)
     (let ((slave-enty (aref ,slave-array x)))
       (when (fix/= 0 slave-enty)		; $OPT:  combine these WHENs.
	 (when (fix/= slave-enty (logand (aref ,master-array x) slave-enty))
	   (return nil))))))

;;; This one is new.  --- Foner 25 Mar 94.
;;; I could just use EQUALP, but this is about 2/3 to 1/2 the runtime,
;;; since we know that the array elements are comparable with EQL
;;; (EQUALP must recurse, and that probably hurts even worse on a
;;; conventional architecture).
(defsubst STATE-ARRAY-EQUAL (sa1 sa2)
  (assert (= (length sa1) (length sa2)))
  (dotimes (x (length sa1) t)
    (unless (eql (aref sa1 x) (aref sa2 x))
      (return nil))))

(defmacro STATE-ARRAY-GET-PRINT-NAME (array)
  `(let ((result ""))
     (dotimes (x *item-number* (string-right-trim "&" result))	; $OPT:  REDUCE maybe?
       (let ((state (state-array-get-state ,array x)))
	 (unless (state-unknown-p state)
	   (setq result
		 (string-append
		   result
		   (when (state-off-p state)
		     "-")
		   (item-print-name (get-item x))
		   "&")))))))

(defmacro STATE-ARRAY-CLEAR (array max-index)	; $OPT:  #+Genera SI:FILL-ARRAY
  `(dotimes (x ,max-index)
     (setf (aref ,array x) (make-state-record))))

(defmacro STATE-ARRAY-COPY (source-array dest-array max-index)	; $OPT:  #+Genera SI:COPY-ARRAY-PORTION
  `(dotimes (x ,max-index)
     (setf (aref ,dest-array x) (aref ,source-array x))))

;;; Why doesn't this use a single loop and STATE-ARRAY-GET-STATE, and why does it
;;; compute things somewhat differently?  Because it wants to print newlines & such.
(defmacro PRINT-STATE-ARRAY-ENABLED (array max-index)
  `(dotimes (x ,max-index)
     (dotimes (y *states-in-fixnum*)
       (format *output-stream* "~A "
	       (state-unparse
		 (state-array-get-state ,array (fix+ y (fix* x *states-in-fixnum*))))))
     (format *output-stream* "~%")))

;;; See discussion at *ITEM-MAXIMUM* for which the original, hardcoded value of 25
;;; was actually too small, but went unnoticed.  This softcoded value is correct.
(defparameter *MICROWORLD-STATE-SIZE* *fixna-required-to-hold-all-item-states*)

(defvar *MICROWORLD-STATE* (make-state-array *microworld-state-size*))

(defmacro GET-MICROWORLD-STATE (index)
  `(state-array-get-state *microworld-state* ,index))

;;;; Rate datatype.

;;; Used to keep track of how often something occurs.
;;; Kept as a ratio of fixnums.
;;; Every time the rate is updated, the denominator is incremented.
;;; The numerator is incremented iff the occurrence happened.
;;; The rate is therefore a fraction giving the rate of occurrence
;;; (i.e. 3/8 means that the situation in question occurred 3 times out
;;; of 8 times total).
;;; To avoid overflow, the maximum value for either denominator or
;;;   numerator is (2^5) - 1 = 31 -- if above this, both are right
;;;   shifted one bit, preserving the ratio (more or less).
;;; A rate therefore consumes 10 bits total, where *RATE-BITS* gives
;;;   the number of bits for either the numerator or denominator.
;;; The initial rate is 0/1, with denominator LSB in the data, this
;;;   simplifies to 1.
;;; Note that all rate functions take an offset, which can be used to
;;;   pack two rates into a single fixnum, or pack a rate plus some
;;;   flags and/or states into a single fixnum.
;;; To compare the magnitude of two rates, think of them as fractions.
;;;   One simply compares NUMERATOR1 * DENOMINATOR2 with NUMERATOR2 *
;;;   DENOMINATOR1 (it should never be possible to have a rate of 0/0,
;;;   as the initial rate is 0/1 and calls to rate-update always
;;;   increment the denominator, so this method always works).
;;; RATE-HIGH-P uses this method, and returns T if rate is greater than 7/8.

(eval-when (load eval compile)			; We need these later in the compilation process for this file, so...
(defconstant *RATE-BITS* 5)			; This is something of a misnomer:  It's really only the number of bits in a numerator or a denominator!
(defconstant *TOTAL-RATE-BITS* (* 2 *rate-bits*))    ; This is better:  it's the number of bits _actually_ occupied by one rate.
)						; EVAL-WHEN.

(defmacro MAKE-RATE ()
  `1)

(defmacro RATE-DENOMINATOR (data offset)
  `(ldb (byte *rate-bits* ,offset) ,data))

(defmacro RATE-NUMERATOR (data offset)
  `(ldb (byte *rate-bits* (fix+ *rate-bits* ,offset)) ,data))

(defun RATE-UNPARSE (data offset)
  (format nil "~D/~D"
          (rate-numerator data offset)
          (rate-denominator data offset)))

(defmacro RATE-UPDATE (data offset occurred)
  `(if ,occurred
       (rate-increment-both ,data ,offset)
       (rate-increment-denominator ,data ,offset)))

(defmacro RATE-INCREMENT-DENOMINATOR (data offset)
  `(let ((value (fix1+ (rate-denominator ,data ,offset))))
     (if (logbitp *rate-bits* value)
         (setf (rate-denominator ,data ,offset) (fixrsh value)
               (rate-numerator ,data ,offset)   (fixrsh (rate-numerator ,data ,offset)))
	 (setf (rate-denominator ,data ,offset) value))))

(defmacro RATE-INCREMENT-BOTH (data offset)
  `(let ((num-value (fix1+ (rate-numerator ,data ,offset)))
         (den-value (fix1+ (rate-denominator ,data ,offset))))
     (if (logbitp *rate-bits* den-value)
         (setf (rate-numerator ,data ,offset)   (fixrsh num-value)
               (rate-denominator ,data ,offset) (fixrsh den-value))
       (setf (rate-numerator ,data ,offset)   num-value
             (rate-denominator ,data ,offset) den-value))))

(defmacro RATE< (data1 offset1 data2 offset2)
  `(fix< (fix* (rate-numerator ,data1 ,offset1)
               (rate-denominator ,data2 ,offset2))
         (fix* (rate-numerator ,data2 ,offset2)
               (rate-denominator ,data1 ,offset1))))

(defmacro RATE-HIGH-P (data offset)
  `(fix> (fix* (rate-numerator ,data ,offset) 8)
         (fix* (rate-denominator ,data ,offset) 7)))

;;;; Weighted-rate datatype.

;;; Used to keep track of how often something occurs, weighted towards
;;; more recent occurrences.
;;; Kept as a short-float which indicates the probability of something
;;; occurring (could be kept as a ratio of fixnums).

(defmacro MAKE-WEIGHTED-RATE ()
  `0.0)

(defmacro WEIGHTED-RATE* (num1 num2)
  `(float* ,num1 ,num2))

(defmacro WEIGHTED-RATE-DECREMENT (num)
  `(setf ,num (float* .7 ,num)))

(defmacro WEIGHTED-RATE-INCREMENT (num)
  `(setf ,num (float+ .3 (float* .7 ,num))))

(defmacro WEIGHTED-RATE-UPDATE (num occurred)
  `(if ,occurred
       (weighted-rate-increment ,num)
     (weighted-rate-decrement ,num)))

(defconstant *WEIGHTED-RATE-HIGH* .875)
(defconstant *WEIGHTED-RATE-MEDIUM* .75)
(defconstant *WEIGHTED-RATE-LOW* .40)

(defmacro WEIGHTED-RATE-HIGH-P (num)
  `(float> ,num *weighted-rate-high*))
(defmacro WEIGHTED-RATE-MEDIUM-P (num)
  `(float> ,num *weighted-rate-medium*))
(defmacro WEIGHTED-RATE-LOW-P (num)
  `(float< ,num *weighted-rate-low*))

;;;; Average datatype.

;;; Used to keep fixnum average durations for synthetic items.
;;; Stored as value and number of samples.

(defstruct (AVERAGE
	     #|| (:PRINT-FUNCTION PRINT-AVERAGE) ||# )
  (value 0  :type fixnum)
  (sample 0 :type fixnum))

;;; Results of the +,*,/ operations not guaranteed to be fixnums.
(defmacro AVERAGE-UPDATE (average new-value)
  `(setf (average-value ,average)
         (floor
          (/ (+ (the fixnum ,new-value)
                (* (the fixnum (average-value ,average))
                   (the fixnum (average-sample ,average))))
             (setf (average-sample ,average)
                   (fix1+ (average-sample ,average)))))))

;;;; Counter datatype.

;;; Used to keep correlations.
;;; The maximum value is 15, and value is always a positive integer.
;;; The positive flag is used to indicate if the value is positive or
;;;   negative.
;;; TOGGLE is used for purposes of tabulating frequency, as follows:
;;; To determine if a correlation exists between an initial and 
;;; final condition, go through a number of trials.
;;; If TOGGLE is true, and the given condition holds, and the final
;;;   condition obtains, then increment the value.
;;; If TOGGLE is false, and the final condition obtains, then
;;;   decrement the value.
;;; Alternate trials (i.e. toggle TOGGLE after each trial) -- as the
;;; decrement is larger than the increment, if the frequency with
;;; which conditions -> final exceeds the frequency in which final
;;; obtains (without checking any conditions) by more than the ratio
;;; of decrement to increment, then the value will eventually max out
;;; at 15 (note: this scheme always exerts pressure towards zero).
;;; Counters are initialized toggle (bit 0) and positive (bit 1) set
;;;   on (both are flags, so on=1 and off=0) and value 0 (bits 2-5).
;;; Each function takes an offset to support putting multiple counters
;;;   or mixed counters and flags/states/rates in the same fixnum.

(defconstant *COUNTER-BITS* 6)
(defconstant *COUNTERS-IN-FIXNUM* (floor *safe-fixnum-length* *counter-bits*))

(deflimit *FIXNA-REQUIRED-TO-HOLD-ALL-ITEM-COUNTERS*
	  (ceiling *item-maximum* *counters-in-fixnum*)
  75)

(deflimit *FIXNA-REQUIRED-TO-HOLD-ALL-CONJ-COUNTERS*
	  (ceiling *conj-maximum* *counters-in-fixnum*)
  50)

(defconstant *COUNTER-MAXIMUM* 15)

(defmacro MAKE-COUNTER ()
  `3)

(defmacro COUNTER (data offset)
  `(ldb (byte *counter-bits* ,offset) ,data))
(defmacro COUNTER-TOGGLE (data offset)
  `(ldb (byte 1 ,offset) ,data))
(defmacro COUNTER-POS (data offset)
  `(ldb (byte 1 (fix1+ ,offset)) ,data))
(defmacro COUNTER-VALUE (data offset)
  `(ldb (byte 4 (fix+ 2 ,offset)) ,data))

(defmacro COUNTER-INCREMENT-VALUE (data offset)
  `(let ((value (counter-value ,data ,offset)))
     (if (fix< value 13)
         (setf (counter-value ,data ,offset) (fix+ value 2))
	 (when (fix/= value 15)
	   (setf (counter-value ,data ,offset) 15)))))

(defmacro COUNTER-DECREMENT-VALUE (data offset)
  `(let ((value (counter-value ,data ,offset)))
     (cond
      ;; 3 or more, simply subtract three
      ((fix> value 2)
       (setf (counter-value ,data ,offset) (fix- value 3)))
      ;; if zero, become -2 (1 correlation in the negative direction)
      ((fix= value 0)
       (setf (counter-pos ,data ,offset)   (make-flag-false)
             (counter-value ,data ,offset) 2))
      ;; otherwise, if 1 or 2, become 0 (positive sign)
      (t
       (setf (counter-pos ,data ,offset)   (make-flag-true)
             (counter-value ,data ,offset) 0)))))

(defmacro COUNTER-MODIFY-VALUE (data offset activated)
  `(if (flag-truep (counter-pos ,data ,offset))
       (if (flag-truep ,activated)
           (counter-increment-value ,data ,offset)
	   (counter-decrement-value ,data ,offset))
       (if (flag-truep ,activated)
	   (counter-decrement-value ,data ,offset)
	   (counter-increment-value ,data ,offset))))

(defmacro COUNTER-TOGGLEP (data offset)
  `(flag-truep (counter-toggle ,data ,offset)))

(defmacro COUNTER-TOGGLE-TOGGLE (data offset)
  `(setf (counter-toggle ,data ,offset)
         (flag-inverse (counter-toggle ,data ,offset))))

(defmacro COUNTER-UNPARSE (data offset)
  (declare (fixnum data offset))
  `(format nil "~A~2D~A"
           (flag-unparse (counter-pos ,data ,offset))
           (counter-value ,data ,offset)
           (if (flag-truep (counter-toggle ,data ,offset))
               "W"
	       "O")))

;;; Counter-record datatype.

;;; Simple way of packing 4 counters together into one fixnum.
;;; COUNTER-RECORD-GET-INITIAL can be called to determine what the
;;; value returned by MAKE-COUNTER-RECORD (a counter record which
;;; initially has all counters set value 0 positive and toggle both
;;; on) should be.

(defconstant *COUNTER-RECORD-MAX-OFFSET*
	     (* *counter-bits* (1- *counters-in-fixnum*)))

(defmacro COUNTER-RECORD-OFFSET (position)
  `(fix* *counter-bits* ,position))

(defmacro MAKE-COUNTER-RECORD ()
  `798915)					; &&& This magic number MUST CHANGE if we ever allow non-24-bit fixna.

(defun COUNTER-RECORD-GET-INITIAL ()		; This apparently returns 798915., e.g., what MAKE-COUNTER-RECORD does, only more slowly.
  (let ((record 0))				; &&& This routine MUST CHANGE (in a pretty simple way) if we ever allow non-24-bit fixna.
    (setf (counter record (counter-record-offset 0)) (make-counter))
    (setf (counter record (counter-record-offset 1)) (make-counter))
    (setf (counter record (counter-record-offset 2)) (make-counter))
    (setf (counter record (counter-record-offset 3)) (make-counter))
    record))

(defun COUNTER-RECORD-UNPARSE (record)		; &&& This routine MUST CHANGE (in a pretty simple way) if we ever allow non-24-bit fixna.
  (format nil "~A  ~A  ~A  ~A"
          (counter-unparse record (counter-record-offset 0))
          (counter-unparse record (counter-record-offset 1))
          (counter-unparse record (counter-record-offset 2))
          (counter-unparse record (counter-record-offset 3))))

;;;; Counter-array datatype.

;;; Used to keep numerous counters.
;;; Perhaps more appropriately called COUNTER-RECORD-ARRAY.

(defmacro MAKE-COUNTER-ARRAY (number)
  `(make-array ,number
               :initial-element (make-counter-record)
               :element-type 'fixnum))

(defmacro GET-COUNTER-ARRAY-INDEX (index)
  `(floor ,index *counters-in-fixnum*))
(defmacro GET-COUNTER-RECORD-POSITION (index)
  `(rem ,index *counters-in-fixnum*))
(defmacro GET-COUNTER-RECORD (array index)
  `(aref ,array ,index))

(defmacro COUNTER-ARRAY-MODIFY-VALUE (array index offset occurred)
  `(counter-modify-value
     (get-counter-record ,array ,index) ,offset ,occurred))

(defmacro COUNTER-ARRAY-VALUE (array index offset)
  `(counter-value
     (get-counter-record ,array ,index) ,offset))

(defmacro COUNTER-ARRAY-POS (array index offset)
  `(counter-pos
     (get-counter-record ,array ,index) ,offset))

(defmacro COUNTER-ARRAY-TOGGLE-TOGGLE (array index offset)
  `(counter-toggle-toggle
     (get-counter-record ,array ,index) ,offset))

(defmacro COUNTER-ARRAY-TOGGLE (array index offset)
  `(counter-toggle
     (get-counter-record ,array ,index) ,offset))

(defmacro COUNTER-UNPARSE-FROM-ARRAY (array index offset)
  `(counter-unparse
     (get-counter-record ,array ,index) ,offset))

;;;; Schema datatype.

;;; Used to store all schemas created by the system.
;;; CONTEXT/ACTION/RESULT "defines" schema (and are used to construct
;;;   its print-name).
;;; Schemas are *never* duplicated.
;;; Schema claims if context met, taking action yields specified
;;;   result (with given reliability factor).
;;; Many statistics concern activation, taking the action of a schema
;;;   when its context is satisfied.
;;; If SYN-ITEM is true, REIFIER gives the syn-item index
;;;   and FIRST-TICK stores a clock tick time for use in updating the
;;;   on and off-durations for the reifying synthetic item.
;;; Otherwise no synthetic item has been created for this schema.
;;; RELIABILITY indicates how often the result obtains when the schema
;;;   is activated (i.e. how often the schema succeeds) and is biased
;;;   towards more recent trials (by using the WEIGHTED-RATE functions).
;;; PARENT gives the schema index of the parent schema;
;;;   -1 if there is no parent (i.e. the schema is blank).
;;; CONTEXT/RESULT-CHILDREN are state-arrays which have a given
;;;   position on/off if this schema has spun off a context/result
;;;   schema with that particular item positively/negatively included.
;;; RESULT-CONJ-CHILDREN is a flag array which has a given
;;;   position true/false if this schema has spun off a result schema
;;;   with that particular conjunction positively included.
;;; The three children structures together keep the mechanism from
;;;   accidentally duplicating an existing schema and also
;;;   CONTEXT-CHILDREN structure supports deferral to a more specific
;;;   schema.
;;; If result is non-empty, EXTENDED-CONTEXT looks for items ON or OFF
;;;   before action which affect probability of success when activated.
;;;   Keeps track of both positive and negative correlations for
;;;     *items* only (not conjunctions).
;;;   Alternate between trials with and without activation of schema
;;;     using the compressed count method described in the thesis.
;;;   If goal-directed-action schema, also keeps same correlations in
;;;     EXTENDED-CONTEXT-POST but with respect to value of items after
;;;     the goal-directed action is executed (currently not implemented).
;;; If RESULT empty, EXTENDED-RESULT looks for item transitions which
;;;   appear to be predicted by the activation of the schema.
;;;   Keeps track of both positive and negative transition
;;;     correlations for primitive items as well as conjunctions
;;;     representing contexts of reliable schemas.
;;;   Alternate between trials with and without activation of schema
;;;     using the compressed count method described in the thesis.
;;;
;;; The data slot holds many important status bits in a compressed
;;;   form.
;;; The APPLICABLE, OVERRIDDEN and ACTIVATED flags are used by the
;;;   toplevel to keep track of the status of the various schemas.
;;; SUCCEEDED-LAST is a flag used to help keep LC-CONSISTENCY.
;;;   Indicates that the schema succeeded last time it was activated.
;;; MARKED is a flag used by the update accessibility routines to keep
;;;   track of which schemas have already been visited by the
;;;   algorithm.
;;; LC-CONSY is a rate used to keep probability of successful
;;;   activation given that the previous activation was successful.
;;; If LC-CONSY is high, the LCLY-CONS flag is set
;;;   true, a synthetic item is created for the schema, and the schema
;;;   mechanism begins to keep track of the on and off duration for
;;;   the synthetic item.

(defstruct (SCHEMA
	     (:CONSTRUCTOR MAKE-SCHEMA-INTERNAL
	      (ACTION-ITEM ACTION))
	     (:PRINT-FUNCTION PRINT-SCHEMA))
  (print-name "" :type string)
  ;; This is a record with all data bits off, except one.  That data bit
  ;; corresponds to a rate of 0/1.  There's no particularly modular way
  ;; to set this without resorting to the explicit setting below, because
  ;; the various macros that define the datatypes happen after the SCHEMA
  ;; structure is defined.  (&&& Though maybe I can just reorder them?)
  (data #.(ash 1 15) :type fixnum)

  ;; Empty context if context-empty is true.
  ;; Single item in context if context-single is true
  ;;   (item is in context-array).
  ;; Multiple items in context if context-single is false.
  ;; Context is *always* in CONTEXT-ARRAY.
  ;; Can also be found as conjunction if CONTEXT-CONJ is true
  ;;   (conj index is in CONTEXT-ITEM).
  (context-array
    (make-state-array *fixna-required-to-hold-all-item-states*)
    :type (vector fixnum *fixna-required-to-hold-all-item-states*))
  (context-item -1 :type fixnum)		; E.g., THIS IS -1 UNLESS IT'S GOT A CONJUCTIVE CONTEXT!!!
  ;; Empty result if result-empty is true.
  ;; Result is conjunction if RESULT-CONJ is true
  ;;   conj index in result-item.
  ;; Otherwise item index is in RESULT-ITEM.
  ;; Item is negated if RESULT-NEGATED is true
  ;; (conjunctions are never negated).
  (result-item -1 :type fixnum)
  ;; Action is goal-directed if ACTION-GD is true
  ;;   goal-directed-action index in action-item (not yet supported).
  ;; Otherwise action index in ACTION-ITEM.
  ;; Action is kept for rapid lookup, could easily be deleted.
  (action #'true   :type compiled-function)
  (action-item -1  :type fixnum)
  (reifier -1      :type fixnum)
  (first-tick -1   :type fixnum)
  (reliability 0.0 :type short-float)
  (parent -1       :type fixnum)
  (context-children
    (make-state-array    *fixna-required-to-hold-all-item-states*)
    :type (vector fixnum *fixna-required-to-hold-all-item-states*))
  (result-children
    (make-state-array    *fixna-required-to-hold-all-item-states*)
    :type (vector fixnum *fixna-required-to-hold-all-item-states*))
  (result-conj-children
    (make-flag-array     *fixna-required-to-hold-all-conj-flags*)
    :type (vector fixnum *fixna-required-to-hold-all-conj-flags*))
  (extended-context
    (make-counter-array  *fixna-required-to-hold-all-item-counters*)
    :type (vector fixnum *fixna-required-to-hold-all-item-counters*))
  (extended-context-post
    (make-counter-array  *fixna-required-to-hold-all-item-counters*)
    :type (vector fixnum *fixna-required-to-hold-all-item-counters*))
  (extended-result-pos
    (make-counter-array  *fixna-required-to-hold-all-item-counters*)
    :type (vector fixnum *fixna-required-to-hold-all-item-counters*))
  (extended-result-neg
    (make-counter-array  *fixna-required-to-hold-all-item-counters*)
    :type (vector fixnum *fixna-required-to-hold-all-item-counters*))
  (extended-result-conj-pos
    (make-counter-array  *fixna-required-to-hold-all-conj-counters*)
    :type (vector fixnum *fixna-required-to-hold-all-conj-counters*))
  (spare-slot-1)				; So I don't necessarily need to recompile to test something...
  (spare-slot-2)				; Ditto.
  )

;;; Data contains important status bits.
;;; pos length
;;; 0   1  context-empty
;;; 1   1  context-single
;;; 2   1  context-conj
;;; 3   1  result-empty
;;; 4   1  result-conj 
;;; 5   1  result-negated
;;; 6   1  result-satisfied
;;; 7   1  action-gd
;;; 8   1  syn-item
;;; 9   1  applicable
;;; 10  1  overridden
;;; 11  1  activated
;;; 12  1  succeeded-last
;;; 13  1  marked
;;; 14  1  lcly-cons (short for locally-consistent)
;;; 15  10 lc-consy (rate) (short for local-consistency)

;;; The macros which take data as an argument do NOT work for
;;; setf; the macros which take schemas as arguments do work for setf.
;;; Note some improvement in performance when using the first form
;;; provided that the data is used for multiple branches/observations.

;;; [This DEFSTORAGE just replaced 185 lines of by-hand code, and makes it more
;;;  likely that that code was correct, too.  Whee!]
(defstorage SCHEMA
  (context-empty    1)
  (context-single   1)
  (context-conj     1)
  (result-empty     1)
  (result-conj      1)
  (result-negated   1)
  (result-satisfied 1)
  (action-gd        1)
  (syn-item         1)
  (applicable       1)
  (overridden       1)
  (activated        1)
  (succeeded-last   1)
  (marked           1)
  (lcly-cons        1)
  (lc-consy         10)
  )

;;; [Another 17 lines of hard-to-read, repetitous code eliminated...]
(def-data-and-instance-macros SCHEMA (lc-consy-unparse)         rate-unparse %%schema-lc-consy-offset)
(def-data-and-instance-macros SCHEMA (lc-consy-update occurred) rate-update  %%schema-lc-consy-offset)
(def-data-and-instance-macros SCHEMA (lc-consy-high-p)          rate-high-p  %%schema-lc-consy-offset)

(defvar *SCHEMA-ARRAY*
	(make-array *schema-maximum*
		    :element-type 'schema))

;;; &&& VARIABLE-PROCLAIMATION PROBLEM.  Temporarily commented out.
; (proclaim '(type (vector schema *schema-maximum*) *schema-array*))

(defmacro GET-SCHEMA (index)
  `(svref *schema-array* (the fixnum ,index)))

;; For when I need this to be functional (e.g., for MAPCAR).  Given that GET-SCHEMA
;; appears to be used only once for SETF, I should just make it a subst and fix the
;; single case where this won't work, but...
(defsubst GET-SCHEMA-FN (index)
  (get-schema index))

(defun PRINT-SCHEMA (schema stream depth)
  depth						; Many things ignore this; it's kosher to do so.
  (flet ((print-it ()
	   (format stream "~A ~15,'0B con ~A"
		   (schema-print-name schema)
		   (mod (schema-data schema)	; Gets rid of higher-order bits.  [&&& In Genera, at least, this turns out to be a push of a constant and a ...
			#.(ash 1 %%schema-lc-consy-offset))    ; ... single instruction, so I doubt this is less efficient than LOGAND with the right mask.]
		   (schema-lc-consy-unparse schema))))
    (print-respecting-flags print-it schema)))

;; Place result of calling this function in the inital data slot for
;; schema -- NOTE: by default, context and result are marked empty.

(defun SCHEMA-GET-INITIAL-DATA ()
  (with-generalized-reference foo
    (setf (schema-data-lc-consy foo) (make-rate))))

;;;; Item datatype.

;;; Used to store information about each primitive and synthetic item.
;;; PRINT-NAME, SYN-ITEM-P and CODE, if primitive, or
;;; SYN-ITEM-INDEX, if synthetic, define the item and never change.
;;; A synthetic item is indicated by having SYN-ITEM-P true.
;;; A primitive item has code bound to the appropriate microworld code
;;;   (see init-item) which returns the state for the item.
;;; GENERALITY is the rate of being on rather than off.
;;; ACCESSIBILITY is the rate of being reachable by a reliable chain
;;;   of schemas beginning with an applicable schema.
;;; GD-CREATED-P indicates if a goal-directed action has been created
;;;   with this item as a goal.

;;; Note: everything commented out below supports goal-directed action
;;; creation for negated items and is not used currently.

;;; This one is used inside an item, though maybe I should define it at SCHEMA instead?
(defun MAKE-SCHEMA-BIT-VECTOR (&optional (size *schema-maximum*))
  (make-array size :element-type 'bit :initial-element 0))

;;; (This one isn't actually used inside an item, but we might as well define it here.)
(defun MAKE-ITEM-BIT-VECTOR (&optional (size *item-maximum*))
  (make-array size :element-type 'bit :initial-element 0))

(defstruct (ITEM
	     (:CONSTRUCTOR MAKE-ITEM-INTERNAL
	      (PRINT-NAME CODE &OPTIONAL	; The optional data is used ONLY by MAKE-ACTION-CAST-AS-ITEM, a gross kluge in itself.
			  CURRENT-STATE LAST-STATE SYN-ITEM-INDEX DATA ACC-DATA
			  CONTEXT-DEPENDENT-SCHEMAS RESULT-DEPENDENT-SCHEMAS))
	     (:PRINT-FUNCTION PRINT-ITEM))
  (print-name     ""                         :type string)
  (current-state  (make-state-unknown)       :type fixnum)
  (last-state     (make-state-unknown)       :type fixnum)
  (code           #'true                     :type compiled-function)
  (syn-item-index -1                         :type fixnum)
  (data           #.(ash 1 3)                :type fixnum)	; This is a single rate of 0/1.
  (acc-data       #.(+ (ash 1 10) (ash 1 0)) :type fixnum)	; This is two packed rates, each 0/1.
  (context-dependent-schemas
    (make-schema-bit-vector))
  (result-dependent-schemas
    (make-schema-bit-vector))
  (spare-slot-1)				; So I don't necessarily need to recompile to test something...
  (spare-slot-2)				; Ditto.
  )

;;; Data contains important status bits.
;;; pos length
;;; 0   1  syn-item
;;; 1   1  gd-pos-created
;;; 2   1  gd-neg-created (currently not used)
;;; 3   10 generality
;;; in acc-data
;;; 0   10 acc-pos
;;; 0   10 acc-neg (currently not used) [&&& Shouldn't this be 10 10?  --- Foner]
;;; note: the stuff not used supports creation of goal-directed actions
;;;   for negated items

;;; The macros which take data as an argument do NOT work for
;;; SETF; the macros which take schemas as arguments do work for SETF.
;;; Note some improvement in performance when using the first form
;;; provided that the data is used for multiple branches/observations.

(defstorage ITEM
  (syn-item       1)
  (gd-pos-created 1)
  (gd-neg-created 1)				; Not currently used, but must be present to keep the fields lined up correctly.
  (generality     *total-rate-bits*)
  )

(def-data-and-instance-macros   ITEM (generality-unparse)         rate-unparse %%item-generality-offset)
(def-data-and-instance-macros   ITEM (generality-update occurred) rate-update  %%item-generality-offset)
(def-data-and-instance-macros-2 ITEM (generality-<)               rate<        %%item-generality-offset)

(defstorage ITEM-ACC
  (pos *total-rate-bits*)
  (neg *total-rate-bits*)
  )

(def-data-and-instance-macros ITEM-ACC (pos-unparse)         rate-unparse %%item-acc-pos-offset)
(def-data-and-instance-macros ITEM-ACC (pos-update occurred) rate-update  %%item-acc-pos-offset)
(def-data-and-instance-macros ITEM-ACC (pos-high-p)          rate-high-p  %%item-acc-pos-offset)

(def-data-and-instance-macros ITEM-ACC (neg-unparse)         rate-unparse %%item-acc-neg-offset)
(def-data-and-instance-macros ITEM-ACC (neg-update occurred) rate-update  %%item-acc-neg-offset)
(def-data-and-instance-macros ITEM-ACC (neg-high-p)          rate-high-p  %%item-acc-neg-offset)

;;; Run these routines once whenever the data structure changes (either
;;; the data structure for item, or the defn of rate), and place results
;;; in the initial slots for data above.

(defun ITEM-DATA-GET-INITIAL ()
  (with-generalized-reference foo
    (setf (item-data-generality foo) (make-rate))))

(defun ITEM-ACC-DATA-GET-INITIAL ()
  (with-generalized-reference foo
    (setf (item-acc-data-pos foo) (make-rate))
    #+gd-actions-negative
    (setf (item-acc-data-neg foo) (make-rate))
    ))

(defvar *ITEM-ARRAY*
	(make-array *item-maximum*
		    :element-type 'item))

;;; &&& VARIABLE-PROCLAIMATION PROBLEM.  Temporarily commented out.
; (proclaim '(type (vector item *item-maximum*) *item-array*))

;;; This is new as of 27 Mar 94.  I finally gave up with having to search
;;; the damned *ITEM-ARRAY* for the print-name of an item.  It's just too
;;; slow, and I have an application now where I need to get this information
;;; fast (e.g., WIN-IF-ON and WIN-IF-OFF slots in goals).  I'm not bothering
;;; to rebind this array etc, because anything that switches worlds will
;;; have to rebuild *ITEM-ARRAY* anyway, so we might as well rebuild this
;;; one, too.
;;;
;;; By fiat, all item symbols are in the SCHEMA: package, not in the package of the
;;; individual microworld (in the same manner that we have only one *ITEM-ARRAY*,
;;; not one per package).  NOTE also that synthetic items do NOT get entered in this
;;; table!  That's because we're ignoring them for the purposes of goals (dealing
;;; with synthetic items & goals would be a whole 'nother ball of wax).
;;;
;;; This is set by MAKE-ITEM.  
(defvar *PRIMITIVE-ITEM-SYMBOL-NAME-TABLE*
	(make-hash-table :name "Primitive item symbol name table"
			 :size *item-maximum*))

(defsubst STORE-PRIMITIVE-ITEM-SYMBOL-NAME (item index pkg)
  (setf (gethash (intern-up (item-print-name item) pkg)
		 *primitive-item-symbol-name-table*)
	index))

;;; Purely for debugging, so I can do this without starting a new run at the moment.
(defun STUFF-*PISNA* (&optional (pkg (find-package 'eyehand)))
  (loop for i from 0 below *item-number*
	for item = (get-item i)
	while (minusp (item-syn-item-index item))	; All synthetic items come at the end, and have nonnegative indices.
	do (store-primitive-item-symbol-name item i pkg))
  (values))

;;; Note:  both forms are given for efficiency.
;;; (When manipulating an item several times, better to get item
;;; directly using get-item rather than repeatedly using the longer form.)

(defmacro GET-ITEM (x)
  `(svref *item-array* (the fixnum ,x)))

(defmacro GET-ITEM-CURRENT-STATE (x)
  `(the fixnum (item-current-state (get-item ,x))))

(defmacro GET-ITEM-LAST-STATE (x)
  `(the fixnum (item-last-state (get-item ,x))))

(defmacro GET-ITEM-CODE (x)
  `(item-code (get-item ,x)))

;;; PKG is NOT optional.  Calling with NIL is trapped.  However, if it is
;;; identically equal to :SYNTHETIC, we're making a synthetic item,
;;; which is NOT stored in *PRIMITIVE-ITEM-SYMBOL-NAME-TABLE*.
(defun MAKE-ITEM (print-name code pkg)
  (assert (not (null pkg)))
  (cond ((fix= *item-number* *item-maximum*)
	 (error "no more space for primitive or synthetic items in item array"))
	(t
	 (setf (get-item *item-number*) (make-item-internal print-name code))
	 (unless (eq pkg :synthetic)
	   (store-primitive-item-symbol-name (get-item *item-number*) *item-number* pkg))
	 (setf *item-number* (fix1+ *item-number*))  ; $OPT:  PROG1?
	 (fix1- *item-number*))))

;;;; Conj datatype.

;;; Used to keep the state of conjunctions of items (primitive or
;;; synthetic).
;;; POSITIVE/NEGATIVE-FLAG-ARRAYS have a flag set if that item is
;;;   included positively/negatively in the item-array.
;;; INCLUSION-ARRAY indicates which other conjs are included by a
;;;   particular conj (i.e. a&b&c includes a&b and a&c, etc).

(defstruct (CONJ
	     (:CONSTRUCTOR MAKE-CONJ-INTERNAL)
	     (:PRINT-FUNCTION PRINT-CONJ))
  (print-name      ""                    :type string)
  (current-state   (make-state-unknown)  :type fixnum)
  (last-state      (make-state-unknown)  :type fixnum)
  (item-array      (make-state-array    *fixna-required-to-hold-all-item-states*)
		   :type (vector fixnum *fixna-required-to-hold-all-item-states*))
  (pos-flag-array  (make-flag-array     *fixna-required-to-hold-all-conj-flags*)
		   :type (vector fixnum *fixna-required-to-hold-all-conj-flags*))
  (neg-flag-array  (make-flag-array     *fixna-required-to-hold-all-conj-flags*)
		   :type (vector fixnum *fixna-required-to-hold-all-conj-flags*))
  (inclusion-array (make-flag-array     *fixna-required-to-hold-all-conj-flags*)
		   :type (vector fixnum *fixna-required-to-hold-all-conj-flags*))
  (data            (ash 1 2)             :type fixnum)	; This is a single rate of 0/1, aligned properly to where rates go in the DATA field.
  (spare-slot-1)				; So I don't necessarily need to recompile to test something...
  (spare-slot-2)				; Ditto.
  )

;;; Data packing similar to item datatype.
;;; pos length
;;; 0   1  gd-pos-created
;;; bit 1 is not used
;;; 2   10 accessibility-pos

;;; The macros which take data as an argument do NOT work for
;;; SETF; the macros which take schemas as arguments do work for SETF.
;;; Note some improvement in performance when using the first form
;;; provided that the data is used for multiple branches/observations.

(defstorage CONJ
  (gd-pos-created    1)
  (unused            1)
  (accessibility-pos *total-rate-bits*)
  )

;;; This is peculiar.  ITEM-ACC stuff starts at 0, but this starts at 2.  I can see why
;;; this might start at 2 (so that we're actually generating code that overlaps the
;;; accessibility-pos stuff defined for CONJ above), but then in that case, why did
;;; the ITEM-ACC stuff start at 0?  Oh, I see:  ITEMs have both a DATA and an ACC-DATA
;;; slot, where as CONJs have only a DATA slot, whose high bits _include_ the
;;; ACC-DATA stuff.  *sigh*
(defstorage CONJ-ACC
  (unused 2)
  (pos    *total-rate-bits*))

;;; There's no actual "object" called a CONJ-ACC.  Instead, it shares space with the
;;; DATA field of a CONJ.  This macro implements the overlap of these two names,
;;; so that the CONJ-ACC-whatever macros defined below actually fetch the data
;;; from the correct place.
(defmacro CONJ-ACC-DATA (ref)
  `(conj-data ,ref))

(def-data-and-instance-macros CONJ-ACC (pos-unparse)         rate-unparse %%conj-acc-pos-offset)
(def-data-and-instance-macros CONJ-ACC (pos-update occurred) rate-update  %%conj-acc-pos-offset)
(def-data-and-instance-macros CONJ-ACC (pos-high-p)          rate-high-p  %%conj-acc-pos-offset)

;;; Run this routine once whenever the data structure changes (either
;;; the data structure for conj, or the defn of rate), and place
;;; result in the initial slot for data above.

(defun CONJ-DATA-GET-INITIAL ()
  (with-generalized-reference foo
    (setf (conj-acc-data-pos foo) (make-rate))))

(defmacro CONJ-INCLUSION-ARRAY-UNPARSE (array)
  `(let ((result ""))
     (dotimes (x *conj-number* (string-right-trim "&" result))	; $OPT:  I think REDUCE would do better here!
       (let ((flag (flag-array-get-flag ,array x)))
	 (when (flag-truep flag)
	   (setq result
		 (string-append
		   result
		   (format nil "~D" x)
		   "&")))))))
  
(defun PRINT-CONJ (conj stream depth)
  depth						; Many things ignore this; it's kosher to do so.
  (flet ((print-it ()
	   (let ((data (conj-data conj)))
	     (safe-format stream "~A:  cur ~A lst ~A acc ~A  ~4A incl ~A"     ; !SF!
			  (conj-print-name conj)
			  (state-unparse (conj-current-state conj))
			  (state-unparse (conj-last-state conj))
			  (conj-acc-data-pos-unparse data)
			  (if (conj-data-gd-pos-created-p data)
			      "ca+" "")
			  (conj-inclusion-array-unparse
			    (conj-inclusion-array conj))))))
    (print-respecting-flags print-it conj)))

(defvar *CONJ-ARRAY*
	(make-array *conj-maximum*
		    :element-type 'conj))

;;; &&& VARIABLE-PROCLAIMATION PROBLEM.  Temporarily commented out.
; (proclaim '(type (vector conj *conj-maximum*) *conj-array*))

;;; Note:  both forms are given for efficiency (when manipulating an
;;; conjunction several times, better to get conjunction directly using
;;; get-conj rather than repeatedly using the longer form) .

(defmacro GET-CONJ (x)
  `(svref *conj-array* (the fixnum ,x)))

(defmacro GET-CONJ-CURRENT-STATE (x)
  `(the fixnum (conj-current-state (get-conj ,x))))

(defmacro GET-CONJ-LAST-STATE (x)
  `(the fixnum (conj-last-state (get-conj ,x))))

(defmacro CONJ-UPDATE-PRINT-NAME (conj)
  `(setf (conj-print-name ,conj)
         (string-append
	   "("
	   (state-array-get-print-name
	     (conj-item-array ,conj))
	   ")")))

(defmacro CONJ-FIND (item-array)
  `(dotimes (x *conj-number* nil)
     (let ((compare-array (conj-item-array (get-conj x))))
       (when (dotimes (y *fixna-required-to-hold-all-item-states* t)	; $OPT:  Oh, ick.  This is basically a slow way to compare bit-arrays. ...
	       (when (fix/= (aref compare-array y) (aref ,item-array y)) ; $OPT: ... It's faster than per-bit, but slower than Ivory block-array hardware.
		 (return nil)))
	 (return x)))))

;;; Inclusion array simply has an ON flag for every included conjunction.
;;; Need to both update all old conjunctions with marks for a new one,
;;; and update the new one with marks for all.
;;; NOTE:  The inclusion array is *only* for conjunctions -- items can
;;; be checked directly by looking at the item array.
;;; NOTE:  This routine expects to be called as the new conjunction has
;;; been added to the conjunction array, but before
;;; *CONJ-NUMBER* has been incremented.

(defmacro CONJ-UPDATE-INCLUSION-ARRAY (conj)
  `(let ((item-array (conj-item-array ,conj))
         (inclusion-array (conj-inclusion-array ,conj))
         (pos *conj-number*))
     (dotimes (x *conj-number*)
       (let* ((current-conj (get-conj x))
	      (current-array (conj-item-array current-conj))
	      (current-inclusion-array
		(conj-inclusion-array current-conj)))
	 ;; update info for old schemas
	 (when (state-array-included-p current-array item-array *fixna-required-to-hold-all-item-states*)
	   (setf (flag-array-get-flag current-inclusion-array pos)
		 (make-flag-true)))
	 ;; update info for newer schema
	 (when (state-array-included-p item-array current-array *fixna-required-to-hold-all-item-states*)
	   (setf (flag-array-get-flag inclusion-array x)
		 (make-flag-true)))))
     ;; set own bit ON as it includes itself
     (setf (flag-array-get-flag inclusion-array pos)
           (make-flag-true))))

;;; MAKE-CONJ returns number of matching existing conjunction,
;;; or creates and returns the number of a new conjunction if needed.
(defun MAKE-CONJ (item-array)
  (if (fix= *conj-number* *conj-maximum*)
      (error "no more space for conjs in conj array")
      (let ((check (conj-find item-array)))
	(if check				; $OPT:  OR
	    check
	    (let ((current-conj
		    (setf (get-conj *conj-number*)
			  (make-conj-internal))))
	      ;; Note that ALL THREE of the forms immediately below should in fact be using ...-ITEM-STATES*,
	      ;; because they're all copying from ITEM-ARRAY, which is declared that way.  (The latter two are
	      ;; copying _into_ flags, but that's handled correctly by the screwy [POS|NEG]-FLAG macros.)
 	      (state-array-copy          item-array (conj-item-array     current-conj) *fixna-required-to-hold-all-item-states*)
	      (state-array-copy-pos-flag item-array (conj-pos-flag-array current-conj) *fixna-required-to-hold-all-item-states*)
	      (state-array-copy-neg-flag item-array (conj-neg-flag-array current-conj) *fixna-required-to-hold-all-item-states*)
	      (conj-update-print-name      current-conj)
	      (conj-update-inclusion-array current-conj)
	      (main-format "~5D conj-created ~D ~A~%"
			   *clock-tick* *conj-number*
			   (conj-print-name current-conj))
	      (setf *conj-number* (fix1+ *conj-number*))	; $OPT:  PROG1
	      (fix1- *conj-number*))))))

;;;; Syn-item datatype.

;;; Synthetic items are used to designate validity conditions of
;;; unreliable schemas which are locally consistent.
;;; The unreliable schema which causes creation is called the
;;; host-schema, while the synthetic item is the schema's "reifier".
;;; Synthetic items are included as items in the item array and are
;;; treated 100% as if they were primitive items (can be in
;;; context/result and conjunctions too).  However, the state of a
;;; synthetic item is determined by the schema mechanism rather than
;;; by a call to a microworld function.
;;; HOST-SCHEMA is the index into the schema array for the host schema.
;;; ITEM-INDEX is the index into the item array for entry for this
;;;   synthetic item.
;;; MAYBE-STATE is used by the routines which calculate the state for
;;;   the synthetic items -- it is merely a place to stash an
;;;   intermediate value.
;;; ON-DURATION and OFF-DURATION is the length of time the synthetic
;;;   item tends to stay on or off once placed in that state.
;;; SET-TIME is the clock tick when the item was last modified.
;;; UNKNOWN-TIME is the clock tick when the item should be
;;;   automatically set unknown.

(defstruct (SYN-ITEM
	     (:CONSTRUCTOR MAKE-SYN-ITEM-INTERNAL
	      (HOST-SCHEMA)))
  (host-schema   -1                   :type fixnum)
  (item-index    -1                   :type fixnum)
  (current-state (make-state-unknown) :type fixnum)
  (maybe-state   (make-state-unknown) :type fixnum)
  (on-duration   (make-average)       :type average)
  (off-duration  (make-average)       :type average)
  (set-time      -1                   :type fixnum)
  (unknown-time  -1                   :type fixnum)
  (spare-slot-1)				; So I don't necessarily need to recompile to test something...
  (spare-slot-2)				; Ditto.
  )

(defvar *SYN-ITEM-ARRAY*
	(make-array *syn-item-maximum*
		    :element-type 'syn-item))

;;; &&& VARIABLE-PROCLAIMATION PROBLEM.  Temporarily commented out.
; (proclaim '(type (vector syn-item *syn-item-maximum*) *syn-item-array*))

;;; Note:  both forms are given for efficiency (when manipulating
;;; several times, better to get syn-item directly rather then
;;; repeatedly using the longer form).

(defmacro GET-SYN-ITEM (x)
  `(svref *syn-item-array* (the fixnum ,x)))

(defmacro GET-SYN-ITEM-CURRENT-STATE (x)
  `(the fixnum (syn-item-current-state (get-syn-item ,x))))

(defun PRINT-SYN-ITEM (item stream)
  (let ((data (item-data item))
	(syn-item (get-syn-item (item-syn-item-index item))))
    (safe-format stream "~A host ~4D item ~4D:  cur ~A lst ~A int ~A gen ~A~%~
                         ~10Ton-dur ~4D off-dur ~4D set ~4D unk ~4D"	 ; !SF!
		 (item-print-name item)
		 (syn-item-host-schema syn-item)
		 (syn-item-item-index syn-item)
		 (state-unparse (item-current-state data))	; Note that this is the current state of the ITEM, not the SYN-ITEM!  Odd...
		 (state-unparse (item-last-state data))
		 (state-unparse (syn-item-current-state syn-item))
		 (item-data-generality-unparse data)
		 (syn-item-on-duration syn-item)
		 (syn-item-off-duration syn-item)
		 (syn-item-set-time syn-item)
		 (syn-item-unknown-time syn-item))))

(defun PRINT-PRIMITIVE-ITEM (item stream)
  (let ((data (item-data item))
	(a-data (item-acc-data item)))
    (safe-format stream
		 #-gd-actions "~A:  cur ~A lst ~A gen ~A acc pos ~A ~A~A~A"	; !SF!
		 #+gd-actions "~A:  cur ~A lst ~A gen ~A acc pos ~A neg ~A ~A~A~A"	; !SF!
		 (item-print-name item)
		 (state-unparse (item-current-state item))
		 (state-unparse (item-last-state item))
		 (item-data-generality-unparse data)
		 (item-acc-data-pos-unparse a-data)
		 #+gd-actions (item-acc-data-neg-unparse a-data)
		 (if (item-data-syn-item-p data)
		     "syn " "")
		 (if (item-data-gd-pos-created-p data)
		     "ca+ " "")
		 #+gd-actions (if (item-data-gd-neg-created-p data)
				  "gd- " "")
		 )))

(defun PRINT-ITEM (item stream depth)
  depth						; Many things ignore this; it's kosher to do so.
  (flet ((print-it ()
	   (if (item-syn-item-p item)
	       (print-syn-item item stream)
	       (print-primitive-item item stream))))
    (print-respecting-flags print-it item)))

(defun MAKE-SYN-ITEM (host-schema)
  (declare (fixnum host-schema))
  (cond ((fix= *syn-item-number* *syn-item-maximum*)
	 (error "no more space for synthetic items in syn-item array"))
	(t
	 (let* ((schema
		  (get-schema host-schema))
		(syn-item
		  (make-syn-item-internal host-schema))
		(item-index
		  (make-item
		    (string-append
		      "["
		      (schema-print-name schema)
		      "]")
		    (compile nil
			     `(lambda ()
				,`(make-state-both)))
		    :synthetic))
		(current-item
		  (get-item item-index)))
	   (setf (item-syn-item current-item)       (make-flag-true)
		 (item-syn-item-index current-item) *syn-item-number*
		 (syn-item-item-index syn-item)     item-index
		 (get-syn-item *syn-item-number*)   syn-item)
	   (main-format "~5D syn item created ~D ~A~%"
			*clock-tick*
			*syn-item-number*
			(item-print-name current-item))
	   (setf *syn-item-number* (fix1+ *syn-item-number*)))))	; $OPT:  PROG1
  (fix1- *syn-item-number*))

;;;; Action datatype.

;;; A simple way to keep the various microworld actions and the
;;; corresponding human-readable print name.

(defvar *ACTION-ARRAY*
	(make-array *action-maximum* :element-type 'compiled-function))

(defvar *ACTION-PRINT-NAME-ARRAY*
	(make-array *action-maximum* :element-type 'string))

;;; &&& VARIABLE-PROCLAIMATION PROBLEM.  Temporarily commented out.
; (proclaim '(type (vector compiled-function *action-maximum*) *action-array*))
;;; &&& VARIABLE-PROCLAIMATION PROBLEM.  Temporarily commented out.
; (proclaim '(type (vector string *action-maximum*) *action-print-name-array*))

(defmacro GET-ACTION (index)
  `(aref *action-array* ,index))

(defmacro GET-ACTION-PRINT-NAME (index)
  `(aref *action-print-name-array* ,index))

;;; This stuff is similar to *PRIMITIVE-ITEM-SYMBOL-NAME-TABLE*, q.v.
(defvar *ACTION-SYMBOL-NAME-TABLE*
	(make-hash-table :name "Action symbol name table"
			 :size *action-maximum*))

(defsubst STRIP-ACTION-NAME (action-name)
  (subseq action-name 1 (1- (length action-name))))

(defsubst STORE-ACTION-SYMBOL-NAME (action-name index pkg)
  (let ((stripped-action-name (strip-action-name action-name)))
    (setf (gethash (intern-up stripped-action-name pkg)
		   *action-symbol-name-table*)
	  index)))

;;; Purely for debugging, so I can do this without starting a new run at the moment.
(defun STUFF-*ASNA* (&optional (pkg (find-package 'eyehand)))
  (loop for i from 0 below *action-number*
	for action-print-name = (get-action-print-name i)
	do (store-action-symbol-name action-print-name i pkg))
  (values))

(defun ALL-ACTION-NAMES ()
  (loop for index from 0 below *action-number*
	collect (strip-action-name (get-action-print-name index))))

(defun MAKE-ACTION (print-name compiled-code pkg)
  (cond ((fix= *action-number* *action-maximum*)
	 (error "no more space for actions in action array"))
	(t
	 (setf (get-action *action-number*)            compiled-code
	       (get-action-print-name *action-number*) print-name)
	 (store-action-symbol-name print-name *action-number* pkg)
	 (setf *action-number*                         (fix1+ *action-number*))	; $OPT:  PROG1
	 (fix1- *action-number*))))

;;;; Miscellaneous functions.

;;; Ramstad never wrote SCHEMA::GOAL-DIRECTED-ACTION-PRINT-NAME
;;; or SCHEMA::GET-GD-ACTION for this function...
(defun SCHEMA-UPDATE-PRINT-NAME (schema)
  (let ((data (schema-data schema))
        (result-item (schema-result-item schema))
        (action-item (schema-action-item schema))
        (context-item (schema-context-item schema)))
    (setf (schema-print-name schema)
	  (string-append
	    (unless (schema-context-empty-p schema)
	      (if (schema-data-context-conj-p data)
		  (conj-print-name
		    (get-conj context-item))
		  (state-array-get-print-name
		    (schema-context-array schema))))
	    (if (schema-data-action-gd-p data)
		#+gd-actions (goal-directed-action-print-name
			       (get-gd-action action-item))
		#-gd-actions (error "Can't get here if goal-directed actions aren't implemented yet!")
		(get-action-print-name action-item))
	    (when (and (not (schema-result-empty-p schema))
		       (not (schema-result-conj-p schema))
		       (schema-result-negated-p schema))
	      "-")
	    (unless (schema-result-empty-p schema)
	      (if (schema-data-result-conj-p data)
		  (conj-print-name
		    (get-conj result-item))
		  (item-print-name (get-item result-item))))))))

(defun MAKE-ACTION-SCHEMA (action-index)
  (if (= *schema-number* *schema-maximum*)
      (error "no more space for schemas")
      (prog1
        (setf (get-schema *schema-number*)
	      (make-schema-internal
		action-index
		(get-action action-index)))
	(setf *schema-number* (fix1+ *schema-number*)))))

(defun MAKE-BLANK-ACTION-SCHEMA (action-index)
  (let ((new-schema (make-action-schema action-index)))
    (setf (schema-result-empty new-schema)  (make-flag-true)
          (schema-context-empty new-schema) (make-flag-true))
    (schema-update-print-name new-schema)
    new-schema))

;;; End of file.
