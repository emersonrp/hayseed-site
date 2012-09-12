;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Eyehand -*-

;;; Microworld implementation

;;; [This file was inspired by Ramstad's implementation of Drescher's
;;; system, but Ramstad's version has by now been 80% or more rewritten.]

;;; This file implements a simple software simulation of a world
;;; it contains variables, definitions and functions for:
;;; Objects in the world and their characteristics.
;;; 140+ functions which can be called to get the status of each
;;;   primitive item.
;;; 17 functions which correspond to the primitive actions.
;;; CLOCK-TICK (a function which randomizes object position, 
;;;   opens the hand after 3 time units of being closed, 
;;;   and increments the clock).
;;; Keeping the state of the world, initializing it, and loading/saving
;;;   the random number generator seed.
;;; [Actually, many of these have been eliminated or moved out of this file.  -- Foner]

;;; Note:  the state of the microworld is kept internally.  Nothing
;;; other than t/nil/status messages are returned by any function.

(in-package :eyehand)

(proclaim '(fixnum *hp-x*   *hp-y*
                   *vp-x*   *vp-y*
                   *clock*  *grip-expiration*))

(proclaim '(atom   *hcl*    *hgr*))
                   
;;; Build the macros we'll need.
(def-microworld EYEHAND)

;;; Note on coordinate systems:
;;; There are three coordinate systems which are used within this
;;; file: microspace, body and glance relative coordinates.
;;; In each case, the X axis (first position in the coordinate pair)
;;; runs left to right while the Y axis (second position) runs bottom
;;; to top, i.e. traditional mathematical notation.

;;; Microspace relative coordinates reference the 7x7 world array
;;; directly where the lower left hand corner is position (0,0) and
;;; the lower right hand corner is position (6,0).

;;; Body relative coordinates are often used when referring to the
;;; center 3x3 area in the microworld.
;;; The lower left corner of this area is microspace coordinate (2,2)
;;; which is defined as body relative coordinate (0,0).

;;; Glance relative coordinates are used when referring to the 5x5
;;; area centered around the current glance orientation.
;;; The current glance orientation is stored as microspace relative
;;; coordinates in *VP-X* and *VP-Y*.
;;; The center of the 5x5 glance relative area is defined as glance
;;; relative coordinate (2,2) with the lower left corner of this area
;;; defined as glance relative coordinate (0,0).

;;; The world is modeled as a 7x7 "microspace" which is 
;;; represented as a 2D array, with indices 0 through 6.

;;; NOTE!  If you change this value, you MUST also change the PROCLAIM
;;; below.  Since *WORLD-DIAMETER* isn't defined until load-time, but
;;; the optimization must be known at compile-time, this is somewhat
;;; problematic unless I arranged to have *WORLD-DIAMETER* in some
;;; previous file just for the PROCLAIM's benefit.
(deflimit *WORLD-DIAMETER* 7 7)

(defvar *WORLD*
  (make-array (list *world-diameter* *world-diameter*)
	      :initial-element nil))

;;; &&& VARIABLE-PROCLAIMATION PROBLEM.  Temporarily commented out.  --- Well, not really, but must fix this case!
;;; The values here must stay in sync with the DEFLIMIT *WORLD-DIAMETER* above!
(proclaim '(type (simple-array t (7 7))
		 *world*))

;;; Brief accessor for readability.
(defmacro GET-OBJECT (x y)
  `(aref *world* ,x ,y))

;;;; World-object datatype.

;;; World objects have three sets of characteristics:
;;; visual, tactile and taste, each of which is represented as
;;; an array of t or nil elements.
;;; In addition, objects can be marked either movable or not.
;;; Currently, only the body and hand are considered to be immovable
;;; objects.
;;; Movable objects can be moved by the hand, and if not currently
;;; grasped will be randomly moved occasionally by the CLOCK-TICK
;;; function in a direction indicated by the current cycle value.
;;; 0 indicates the next move is to the right, 1 down, 2 left, 3 up.
;;; i.e. the objects move in a clockwise direction over time.

(defstruct WORLD-OBJECT
  (visual
   (make-array 16 :initial-element nil)
   :type (simple-array atom (16)))
  (tactile
   (make-array 4 :initial-element nil)
   :type (simple-array atom (4)))
  (taste
   (make-array 4 :initial-element nil)
   :type (simple-array atom (4)))
  (movable t :type atom)
  (cycle 0 :type fixnum))

;;; Brief accessors for readability.

(defmacro GET-VISUAL (object index)
  (declare (fixnum index))
  `(the atom (svref (world-object-visual ,object) ,index)))

(defmacro GET-TACTILE (object index)
  (declare (fixnum index))
  `(the atom (svref (world-object-tactile ,object) ,index)))

(defmacro GET-TASTE (object index)
  (declare (fixnum index))
  `(the atom (svref (world-object-taste ,object) ,index)))

(defmacro GET-MOVABLE (object)
  `(the atom (world-object-movable ,object)))

;;; This definition seems to give SETF the willies, at least in HCL.
; (defmacro GET-CYCLE (object)
;   `(the fixnum (world-object-cycle ,object)))

(defmacro GET-CYCLE (object)
  `(world-object-cycle ,object))

;;;; Defining the body and hand objects.

(defvar *BODY*
  (make-world-object
   :visual   (make-array 16
                         :initial-contents
                         '(t   t   t   t
                           t   nil nil t
                           t   nil nil t
                           t   t   t   t))
   :tactile  (make-array 4
                         :initial-contents
                         '(t   t   nil nil))
   :taste    (make-array 4
                         :initial-contents
                         '(t   nil nil nil))
   :movable nil))

(defvar *HAND*
  (make-world-object
   :visual   (make-array 16
                         :initial-contents
                         '(t   t   t   t
                           t   t   t   t
                           nil nil t   t
                           t   t   t   t))
   :tactile  (make-array 4
                         :initial-contents
                         '(t   nil t   nil))
   :taste    (make-array 4
                         :initial-contents
                         '(t   t   t   nil))
   :movable nil))

;;;; Defining the left and right hand objects.

(defvar *OBJECT-1*
  (make-world-object
   :visual   (make-array 16
                         :initial-contents
                         '(nil t   t   nil
                           t   nil nil t
                           t   nil nil t
                           nil t   t   nil))
   :tactile  (make-array 4
                         :initial-contents
                         '(nil nil t   t))
   :taste    (make-array 4
                         :initial-contents
                         '(nil t   t   nil))
   :movable t  :cycle 0))

(defvar *OBJECT-2*
  (make-world-object
   :visual   (make-array 16
                         :initial-contents
                         '(t   t   nil nil
                           nil t   t   nil
                           nil nil t   t
                           nil nil nil t))
   :tactile  (make-array 4
                         :initial-contents
                         '(nil t   nil nil))
   :taste    (make-array 4
                         :initial-contents
                         '(nil t   nil t  ))
   :movable t  :cycle 1))

(defun COMPARE-DETAILS (&key (objects (list *body* *hand* *object-1* *object-2*))
			(accessor #'world-object-visual))
  (mapcar #'(lambda (object)
	      (listarray
		(funcall accessor object)))
	  objects))

(defun SHOW-COMPARE-DETAILS (&key (objects (list *body* *hand* *object-1* *object-2*))
			     (accessor #'world-object-visual))
  (let ((details (compare-details :objects objects :accessor accessor)))
    (loop for detail in details
	  do (format t "~&~S~&"
		     (mapcar #'(lambda (value)
				 (if value 1 0))
			     detail))))
  (values))

;;; Note on the body relative coordinate system:
;;; body relative coordinates are often used when referring to the
;;; center 3x3 area in the microworld.
;;; The lower left corner of this area is defined as body relative
;;; coordinate (0,0) which corresponds to microspace coordinate (2,2).

;;; The position of the body in body relative coordinates is (1,-1)
;;; which corresponds to microspace coordinate (3,1).

(defconstant *BODY-X* 1)
(defconstant *BODY-Y* -1)

;;; *CLOCK* is incremented with each call to the function CLOCK-TICK.
;;; *GRIP-EXPIRATION* is used by the CLOCK-TICK function to ensure
;;; that the hand is not closed for more than 3 consecutive time units.

(defvar *CLOCK* 0)
(defvar *GRIP-EXPIRATION* -1)

;;;;  *** start of primitive item definitions ***
;;;  The primitive items are implemented as functions which return
;;;  values which indicate the current state of a particular item.
;;;  The value T indicates the item is currently on.
;;;  The value NIL indicates the item is currently off.
;;;  Collectively, they indicate those states of the microworld
;;;  which the simulated robot is currently able to sense.

;;; I'd like to make the four macros below usable across packages without just
;;; copying their code, a la what I did with DEF-DEFITEM, but, unlike the four
;;; macros defined by DEF-DEFITEM, each macro requires the one before it
;;; to expand correctly, and this means I can't just make a form that expands into a
;;; bunch of DEFMACRO forms, since those would be expanded in parallel.  I could
;;; probably find a way around this, but it seems like too much work right now.

;;; Note that the ERROR-CHECKING-FORM used by DEF-ITEMS-INTERNAL via
;;; DEF-ITEM-GENERATOR (and hence available in generators defined by DEF-ITEMS-1D
;;; and DEF-ITEMS-2D) is presumably some ASSERT form which will fail if, for
;;; example, a limit is set wrong.  We evaluate it _after_ doing the internal DEFLIMIT
;;; (since otherwise we really don't have an appropriate scope for the form to use
;;; in checking anything---the limit variable isn't set yet, and CURRENT-LIMIT isn't
;;; bound here), which means that, if the assertion blows out, the limit has
;;; _already_ been changed (possibly propagating changes to other limits, too).  This
;;; shouldn't be a problem if limit initialization forms don't have side-effects (which
;;; it is documented that they shouldn't), and if you eventually fix whatever problem
;;; caused the assertion to blow out and reevaluate the DEF-ITEMS-2D (or whatever)
;;; form (which you presumably will, since it blew out in the middle of compiling it).
;;;
;;; Note that we _also_ pass the ERROR-CHECKING-FORM to DEFLIMIT itself to check.
;;; So if that inner check blows out, we won't wind up making any other
;;; side-effects.  (Although it's certainly possible to write some assertion that
;;; doesn't check the inner use of DEFLIMIT at all, but still blows out in the outer
;;; check, after other things have been done---though I expect this to be rather
;;; unlikely.  But just in case, we also check in DEF-ITEMS-INTERNAL as well as giving
;;; the form to DEFLIMIT.)

;;; Used by DEF-ITEM-GENERATOR below.
(defmacro-definer DEF-ITEMS-INTERNAL (limit-name current-limit historical-limit base-name error-checking-form &body builder-body)
  `(progn
     (deflimit ,limit-name ,current-limit ,historical-limit ,error-checking-form)
     (def-simple-item-naming-function ,base-name)
     ,(when error-checking-form
	error-checking-form)
     ,@builder-body))

;;; Builds a macro named GENERATOR-NAME whose arglist is as shown by the line with
;;; the "***" comment below.  The body therefore gets those particularly-named
;;; args to work with.
(defmacro-definer DEF-ITEM-GENERATOR (generator-name &body generator-body)
  `(defmacro ,generator-name (limit-name current-limit historical-limit base-name item-fn &optional error-checking-form); *** Generator's arglist.
     (let* ((namer 
	      (intern-format "simple-item-naming-function-~A" base-name))	; Hmm.  I don't like have to mention it explicitly like this...
	    (forms 
	      ,@generator-body))
       `(progn
	  (def-items-internal ,limit-name ,current-limit ,historical-limit ,base-name ,error-checking-form
	    ,@forms)))))

;;; Note that it's actually possible to recompile a DEF-ITEMS-1D or DEF-ITEMS-2D
;;; form without destroying the state of the world, subject to a few conditions:
;;;   The new number of items generated by the form is not less than the old number.
;;;   (This is because nothing will remove the no-longer-generated items from the list
;;;   in *PRIMITIVE-ITEMS*, so they'll continue to exist in *ITEM-ARRAY* and get called
;;;   by the schema mechanism.)
;;;   You make sure that the next run you do starts a new run (rather than continuing
;;;   an old one), so MAKE-ALL-ITEMS runs, which will properly update various datastructures,
;;;   such as *NUMBER-OF-PRIMITIVE-ITEMS*.

;;; To solve the problem in the first limitation above, I suppose that I could make a
;;; form that zaps all the items with names "similar to" those generated by the
;;; simple-item-naming-function, to be used before recompiling one of these forms
;;; in a way that would try to decrease the number of items generated, for use in
;;; patch files.  I don't feel comfortable having _every_ use of one of these
;;; generators calling it, though (hence I shouldn't make it part of the
;;; macroexpansion of the generator).
;;;
;;; To solve the problem in the second limitation above, I suppose I could make any
;;; use of these forms set a global variable that warns and/or forces a new run
;;; (unless I happen to be clever & careful enough to know that what I've done is no
;;; cause for a new run, and reset it).  This might be justified.

(def-item-generator DEF-ITEMS-1D
  (loop for index from 0 below current-limit
	collect `(defitem-built ,item-fn ,namer ,index)))

(def-item-generator DEF-ITEMS-2D 
  (loop for x from 0 below current-limit
	append (loop for y from 0 below current-limit
		     collect `(defitem-built ,item-fn ,namer ,x ,y))))

;;;; Haptic-proprioceptive items hp00-hp22
;;; The variables *HP-X* and *HP-Y* store the current body relative
;;; hand position - each ranges from 0-2.

(defvar *HP-X* 1)
(defvar *HP-Y* 0)

;;; HP-CHECK returns T if X and Y give the current hand position.

(defmacro HP-CHECK (x y)
  (declare (fixnum x y))
  `(and
    (fix= *hp-x* ,x)
    (fix= *hp-y* ,y)))

;;; Definition of the HP item functions via the HP-CHECK macro.

(def-items-2d *HP-DIAMETER* 3 3 hp hp-check)

;;;; Visual-proprioceptive items vp00-vp22.
;;; The variables *VP-X* and *VP-Y* store the current glance
;;; orientation - each ranges from 0-2.
;;; They are microspace relative coordinates which designate the
;;; center of the 5x5 visual field.

(defvar *VP-X* 1)
(defvar *VP-Y* 1)

;;; VP-CHECK returns T if X and Y give the current glance orientation.

(defmacro VP-CHECK (x y)
  (declare (fixnum x y))
  `(and
    (fix= *vp-x* ,x)
    (fix= *vp-y* ,y)))

;;; Definition of the VP item functions via the VP-CHECK macro.

(def-items-2d *VP-DIAMETER* 3 3 vp vp-check)

;;; Note on the glance relative coordinate system:
;;; glance relative coordinates are used when referring to the 5x5
;;; area centered around the current glance orientation.
;;; The center of this area is defined as glance relative coordinate
;;; (2,2) with the lower left corner of this area defined as glance
;;; relative coordinate (0,0).
;;; The current glance orientation is stored as microspace relative
;;; coordinates in *VP-X* and *VP-Y*.

;;;; Coarse visual-field items vf00-vf44.

;;; VF-CHECK returns T if an object is at the given
;;; glance relative position - if empty, returns NIL.

(defmacro VF-CHECK (x y)
  (declare (fixnum x y))
  `(if (get-object (fix+ *vp-x* ,x) (fix+ *vp-y* ,y))
       t nil))

;;; Definition of the vf item functions via the VF-CHECK macro.

(def-items-2d *VF-DIAMETER* 5 5 vf vf-check
	      (assert (oddp *vf-diameter*)))	; Otherwise, we don't know where to center the fovea!

;;;; Visual detail items:
;;;; fovf00-33, fovb00-33, fovl00-33, fovr00-33, fovx00-33.

;;; Each FOV-CHECK returns one of the visual details
;;; associated with the object found at the given foveal
;;; area - each detail is either T or NIL.
;;; Note that the foveal areas are determined by the current
;;; microspace relative glance orientation.

;;; Note that the 2D items are kept internally in a 1D array so it is
;;; necessary to translate between the two representations when
;;; referencing the get-visual macro.
;;; Further note that this translation happens at compile time, not
;;; run time, so it doesn't slow down the execution of the code.

(defmacro FOVF-CHECK (x y)
  (declare (fixnum x y))
  `(let ((foo (get-object (fix+ 2 *vp-x*) (fix+ 3 *vp-y*))))
     (and foo (get-visual foo ,(+ (* x 4) y)))))

(defmacro FOVB-CHECK (x y)
  (declare (fixnum x y))
  `(let ((foo (get-object (fix+ 2 *vp-x*) (fix1+ *vp-y*))))
     (and foo (get-visual foo ,(+ (* x 4) y)))))

(defmacro FOVL-CHECK (x y)
  (declare (fixnum x y))
  `(let ((foo (get-object (fix1+ *vp-x*) (fix+ 2 *vp-y*))))
     (and foo (get-visual foo ,(+ (* x 4) y)))))

(defmacro FOVR-CHECK (x y)
  (declare (fixnum x y))
  `(let ((foo (get-object (fix+ 3 *vp-x*) (fix+ 2 *vp-y*))))
     (and foo (get-visual foo ,(+ (* x 4) y)))))

(defmacro FOVX-CHECK (x y)
  (declare (fixnum x y))
  `(let ((foo (get-object (fix+ 2 *vp-x*) (fix+ 2 *vp-y*))))
     (and foo (get-visual foo ,(+ (* x 4) y)))))

(def-items-2d *FOVF-DIAMETER* 4 4 fovf fovf-check)
(def-items-2d *FOVB-DIAMETER* 4 4 fovb fovb-check)
(def-items-2d *FOVL-DIAMETER* 4 4 fovl fovl-check)
(def-items-2d *FOVR-DIAMETER* 4 4 fovr fovr-check)
(def-items-2d *FOVX-DIAMETER* 4 4 fovx fovx-check)

;;;; Coarse tactile items for each side of the hand:
;;;; tactf, tactb, tactr, tactl.

;;; Return T if an object is present, NIL if not.
;;; T or NIL is returned explicitly so the user cannot 'accidentally'
;;; get an object structure.

;;; Note that the object positions checked are the microspace
;;; coordinates indicated by the current body relative position of the
;;; hand.

(defitem TACTF
  (if (get-object (fix+ 2 *hp-x*) (fix+ 3 *hp-y*))
      t nil))

(defitem TACTB
  (if (get-object (fix+ 2 *hp-x*) (fix1+ *hp-y*))
      t nil))

(defitem TACTR
  (if (get-object (fix+ 3 *hp-x*) (fix+ 2 *hp-y*))
      t nil))

(defitem TACTL
  (if (get-object (fix1+ *hp-x*) (fix+ 2 *hp-y*))
      t nil))

;;;; Coarse tactile items for each side of the body;
;;;; bodyf, bodyb, bodyr, bodyl.

;;; Again, return T if an object is present, NIL if not.
;;; T or NIL is returned explicitly so the user cannot 'accidentally'
;;; get an object structure.

;;; Note that the object positions checked are the microspace
;;; coordinates indicated by the current body relative position of the
;;; body.

(defitem BODYF
  (if (get-object (fix+ 2 *body-x*) (fix+ 3 *body-y*))
      t nil))

(defitem BODYB
  (if (get-object (fix+ 2 *body-x*) (fix1+ *body-y*))
      t nil))

(defitem BODYR
  (if (get-object (fix+ 3 *body-x*) (fix+ 2 *body-y*))
      t nil))

(defitem BODYL
  (if (get-object (fix1+ *body-x*) (fix+ 2 *body-y*))
      t nil))

;;;; Detailed tactile items for objects touching the left edge (i.e.
;;;; "fingers") of the hand:  text0-3.

;;; Each detail is T or NIL.

;;; Note that the position checked is the microspace coordinate
;;; indicated as the position to the left of the current body relative
;;; position of the hand.

(defmacro TEXT-CHECK (x)
  (declare (fixnum x))
  `(let ((foo (get-object (fix1+ *hp-x*) (fix+ 2 *hp-y*))))
     (when foo
       (get-tactile foo ,x))))

(def-items-1d *TEXT-DIAMETER* 4 4 text text-check)

;;;; Detailed taste items for objects touching the front (i.e. "mouth")
;;;; of the body:  taste0-3.

;;; Each detail is T or NIL.

;;; Note that the position checked is the microspace coordinate
;;; indicated as the position to the front of the current body relative
;;; position of the body.

(defmacro TASTE-CHECK (x)
  (declare (fixnum x))
  `(let ((foo (get-object (fix+ 2  *body-x*) (fix+ 3 *body-y*))))
     (when foo
       (get-taste foo ,x))))

(def-items-1d *TASTE-DIAMETER* 4 4 taste taste-check)

;;;; Hand closed item:  hcl.

(defvar *HCL* nil)

(defitem HCL
  *hcl*)

;;;; Hand closed and grasping something item:  hgr.

(defvar *HGR* nil)

(defitem HGR
  *hgr*)

;;;;  *** start of primitive action definitions ***
;;;  The primitive actions are implemented as functions which may or
;;;  may not change the microworld state.
;;;  These functions do not return meaningful values.

;;;; Hand motion functions:  handf, handb, handr, handl.

;;; Each function first checks the new position for conflicts
;;; (out of the 3x3 range, object already occupying new position)
;;; and if none, moves the hand by first moving any grasped object,
;;; and then moving the hand.
;;; Note that the destination square for both the hand and any grasped
;;; object must be empty for the hand movement to take place
;;; (however, when the hand is closed and grasping an object and moves
;;; left, the hand will occupy the former position of the grasped
;;; object so only the position left of the object needs to be empty,
;;; and similarly for moving right with a grasped object, the object
;;; will occupy the former position of the hand so only the position
;;; to the right of the hand needs to be empty).

;;; Stolen from SCHEMA::FAKE-NEW-LOCATION.  I probably shouldn't have two copies
;;; of the code, but the SCHEMA version is a hack anyway.
;;;
;;; Computes where the new location will be.
(defun DIRECTION->NEW-LOCATION (direction x y)
  #+Genera (declare (values new-x new-y))
  (ecase direction
    (:up    (values x      (1+ y)))
    (:down  (values x      (1- y)))
    (:left  (values (1- x) y))
    (:right (values (1+ x) y))))

;;; %%% Where to go from here:  (This comment written 6 Nov, but it's about
;;; code I wrote in early Oct...)  use DIRECTION->NEW-LOCATION to compute where
;;; things are for HANDx stuff.  Test using the world-state-shower I wrote, whatever
;;; its name is.  Make sure objects move correctly as well as the hand.
;;;
;;; Then, emit opcodes for hand & object motion.  Do same for eye.  Do same for
;;; grasp/ungrasp.  Make a meter to hold all this stuff.  Probably should put all
;;; events that happen in one action in same cluster (e.g., hand motion and object
;;; motion), since I can always flatten it out later, and this way the meter's entries
;;; map one-to-one with clock ticks.
;;;
;;; Note also that we define a meter below (*INANIMATE-OBJECT-MOTIONS*) and
;;; use it in CLOCK-TICK-MAYBE-MOVE-OBJECTS for the circular object motions.
;;; Should flush that meter and just add such motions to the meter we define
;;; for dealing with this stuff here.

;;; Need to compute new coords and store them in the meter!
(defaction HANDF
  (unless (or (fix> *hp-y* 1)
	      (get-object (fix+ 2 *hp-x*) (fix+ 3 *hp-y*))
	      (and *hgr* (get-object (fix1+ *hp-x*) (fix+ 3 *hp-y*))))
    (when *hgr*
      (setf (get-object (fix1+ *hp-x*)  (fix+ 3 *hp-y*)) (get-object (fix1+ *hp-x*)  (fix+ 2 *hp-y*))
	    (get-object (fix1+ *hp-x*)  (fix+ 2 *hp-y*)) nil))
    (setf   (get-object (fix+ 2 *hp-x*) (fix+ 3 *hp-y*)) (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*))
	    (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*)) nil
  	    *hp-y* (fix1+ *hp-y*))))

(defaction HANDB
  (unless (or (fix< *hp-y* 1)
	      (get-object (fix+ 2 *hp-x*) (fix1+ *hp-y*))
	      (and *hgr* (get-object (fix1+ *hp-x*) (fix1+ *hp-y*))))
    (when *hgr*
      (setf (get-object (fix1+ *hp-x*)  (fix1+ *hp-y*))  (get-object (fix1+ *hp-x*)  (fix+ 2 *hp-y*))
	    (get-object (fix1+ *hp-x*)  (fix+ 2 *hp-y*)) nil))
    (setf   (get-object (fix+ 2 *hp-x*) (fix+ 1 *hp-y*)) (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*))
	    (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*)) nil
	    *hp-y* (fix1- *hp-y*))))

(defaction HANDR
  (unless (or (fix> *hp-x* 1)
	      (get-object (fix+ 3 *hp-x*) (fix+ 2 *hp-y*)))
    (setf (get-object (fix+ 3 *hp-x*) (fix+ 2 *hp-y*))
	  (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*)))
    (cond (*hgr*
	   (setf (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*)) (get-object (fix1+ *hp-x*) (fix+ 2 *hp-y*))
		 (get-object (fix1+ *hp-x*)  (fix+ 2 *hp-y*)) nil))
	  (t
	   (setf (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*)) nil)))
    (setf *hp-x* (fix1+ *hp-x*))))

(defaction HANDL
  (unless (or (fix< *hp-x* 1)
	      (and *hgr* (get-object *hp-x* (fix+ 2 *hp-y*)))
	      (and (not *hgr*)
		   (get-object (fix1+ *hp-x*) (fix+ 2 *hp-y*))))
    (when *hgr*
      (setf (get-object *hp-x*          (fix+ 2 *hp-y*)) (get-object (fix1+ *hp-x*)  (fix+ 2 *hp-y*))))
    (setf   (get-object (fix+ 1 *hp-x*) (fix+ 2 *hp-y*)) (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*))
	    (get-object (fix+ 2 *hp-x*) (fix+ 2 *hp-y*)) nil
	    *hp-x* (fix1- *hp-x*))))

;;;; Glance orientation functions:  eyef, eyeb, eyer, eyel.

;;; Each function merely checks to make sure that the glance
;;; orientation variable will still be in the range 0-2 inclusive
;;; after modification, and then performs the modification.

;;; This is the "clearance" between the diameter of the course visual retina and the
;;; diameter of the microworld itself (the former is nested inside the latter like a
;;; planetary gear).  Thus, we cannot move the retina more than this clearance from
;;; the 0,0 point without having it hang off an edge.
(deflimit *WORLD-VF-CLEARANCE* (- *world-diameter* *vf-diameter*) 2
	  ;; VF must be no larger than the world.  (If it's the same size, we can't
	  ;; shift our gaze at all---'cause there's no where else to look---but
	  ;; that's okay, if pathological.  But having it larger means we're looking
	  ;; past the edges of the universe!)
	  (assert (not (minusp *world-vf-clearance*))))
  
(defaction EYEF
  (when (fix< *vp-y* *world-vf-clearance*)
    (setf *vp-y* (fix1+ *vp-y*))))

(defaction EYEB
  (when (fix> *vp-y* 0)
    (setf *vp-y* (fix1- *vp-y*))))

(defaction EYER
  (when (fix< *vp-x* *world-vf-clearance*)
    (setf *vp-x* (fix1+ *vp-x*))))

(defaction EYEL
  (when (fix> *vp-x* 0)
    (setf *vp-x* (fix1- *vp-x*))))

;;;; Hand closing and opening functions:  grasp, ungrasp.

;;; This function closes the hand, grasping any movable object
;;; touching the left edge of the hand (unless hand was already closed).
;;; The hand stays closed until 3 time units pass or until explicitly
;;; opened.

(defaction GRASP
  (unless *hcl*
    (let ((object (get-object (fix1+ *hp-x*) (fix+ 2 *hp-y*))))
      (when (and object
		 (get-movable object))
	(setf *hgr* t)))
    (setf *hcl* t
	  *grip-expiration* (fix+ 3 *clock*))))

;;; This function opens the hand.
(defaction UNGRASP
  (setf *hcl* nil
        *hgr* nil
        *grip-expiration* -1))

;;;; Time-keeping function CLOCK-TICK

;;; Each time unit should correspond to one primitive action taken.
;;; This function should be called after each primitive action and
;;; before the schema mechanism takes the statistics for this time
;;; step - it returns the new value of *CLOCK*.
;;; The grip expires and the hand automatically opens after 3 time
;;; units from when it was first closed
;;; (i.e. if grasp is the action selected at time x, the hand will
;;; remain closed for time x+1, time x+2 and time x+3, unless
;;; explicitly opened during one of these time steps.  If it is not
;;; explicitly opened, after the action is executed for time x+3, the
;;; following call to clock-tick will open the hand and set the time
;;; equal to x+4).
;;; On average, every 200 calls to this function randomly moves all of
;;; the non-grasped movable objects in the world (unless moving a
;;; particular object would result in a collision with another object).
;;; (1:200 -> 1,000:200,000 for additional randomness.)
;;; The direction moved is indicated by the current cycle value for
;;; the object in question.
;;; 0 indicates the next move is to the right, 1 down, 2 left, 3 up.
;;; I.e., the objects move in a clockwise direction over time.

;;; The stuff on this page was totally rewritten on 28 Sep 93 to break out the logic
;;; for object motions and record it for later playback in an animation.

(schema::def-vector-metering-counter *inanimate-object-motions*)

;;; Unbelievably enough, an individual object has no idea what it is!  So we'll look it
;;; up, erring if it's nothing we know.  *sigh*  I could fix this, but then dumped
;;; worlds couldn't be reloaded, probably.  We can't use EQ to check, either, because
;;; the act of restoring a snapshot creates a _copy_ of the object which is no longer
;;; EQ to the original.  So taste them instead.  (This depends on them tasting different,
;;; of course, but happens to be true here.  A new cut at this, or any other microworld,
;;; would explicitly represent object identities, not for use by items [that'd be cheating],
;;; but for use by monitoring, metering, and debugging code, which is what we have here.
(defun IDENTIFY-OBJECT (object)
  (let ((taste (world-object-taste object)))
    (loop for object in (list *object-1* *object-2* *hand* *body*)	; This conses four words per test.  So sue me.
	  for name in '(:object-1 :object-2 :hand :body)
	  when (equalp taste (world-object-taste object))
	    do (return name))))

;;; A paranoid version that'll blow out if not handed one of *OBJECT-1* or *OBJECT-2*.
(defun IDENTIFY-INANIMATE-OBJECT-ONLY (object)
  (let ((id (identify-object object)))
    (unless (member id '(:object-1 :object-2))
      (error "~S is identified as ~S, but it must be one of :OBJECT-1 or :OBJECT-2."
	      object id))
    id))

;;; &&& Yuck!  This apparently just scans the entire world looking for objects.
;;; &&& Surely, as I scale up the world, this should be replaced by something
;;; &&& that tracks the objects directly, since otherwise it gets O(n^2) slower.
(defun CLOCK-TICK-MAYBE-MOVE-OBJECTS ()
  (flet ((do-move (object old-x old-y new-x new-y)
	   (setf (get-object new-x new-y) object)
	   (setf (get-object old-x old-y) nil)
	   (setf (get-cycle object) (mod (1+ (get-cycle object)) 4))
	   (push (list (identify-inanimate-object-only object) *clock* new-x new-y)
		 *inanimate-object-motions*))
	 (compute-move (cycle x y)
	   #+Genera (declare (values new-x new-y move-legal?))
	   (ecase cycle
	     (0 (values (fix1+ x) y         (< x *world-diameter*)))
	     (1 (values x         (fix1- y) (> y 0)))
	     (2 (values (fix1- x) y         (> x 0)))
	     (3 (values x         (fix1+ y) (< y *world-diameter*))))))
    (dotimes (x *world-diameter*)
      (declare (fixnum x))
      (dotimes (y *world-diameter*)
	(declare (fixnum y))
	(let ((object (get-object x y)))
	  (when (and object
		     ;; &&& I'd like to fix this to call random AFTER we check whether we can move the object,
		     ;; but I can't do that until I re-evolve where all the objects are in a run for the demo animation.
		     ;; (This would run a whole lot faster if we didn't call random every loop, but only when necessary.)
		     ;; It would also mean that all simulations after that point would pick different actions, depending on
		     ;; how the world ran that time, hence things might diverge on the action front anyway.  (And it's
		     ;; for sure that runs before & after this change would pick different actions, since we wouldn't be
		     ;; calling RANDOM 4 times (here---see COUNT-OBJECTS-IN-WORLD) in between each call to it to pick
		     ;; an action.  *sigh*
		     (fix< (random 200000) 1000)
		     (not (and (fix= x (fix1+  *hp-x*))
			       (fix= y (fix+ 2 *hp-y*))
			       *hgr*))
		     (get-movable object))
	    (multiple-value-bind (new-x new-y legal?)
		(compute-move (get-cycle object) x y)
	      (when (and legal?			; Won't move it past the end of the world.
			 (not (get-object new-x new-y)))	; Won't land on some other object.
		(do-move object x y new-x new-y)))))))))

(defun CLOCK-TICK ()
  (clock-tick-maybe-move-objects)
  (when (and *hcl*
	     (fix= *clock* *grip-expiration*))
    (ungrasp))
  (setf *clock* (fix1+ *clock*)))

;;; +++ Debugging function only from here to the end of the page.

;;; OBJECT-1 should be the left object (from inspection of the ancient code's init-world stuff).

;;; Restores ONLY the microworld state, not the schema state.
;;; [This would be better written as SCHEMA::(...), but Harlequin apparently
;;; chokes if a newline or a paren follows a colon.  So do it the hard way...]
(defun SCHEMA::RESTORE-EYEHAND-ONLY (snapshot-spec)
  (let ((snapshot (schema::snapshot-from-spec snapshot-spec)))
    (schema::with-snapshot-destructuring snapshot
      schema::schema-system-version schema::timestamp schema::clock-tick
      schema::metering-state schema::schema-state schema::random-state schema::comment	; Ignored.
      (eyehand::safe-restore-eyehand-state schema::microworld-state))))

(defun TICKER-RESET ()
  (schema::restore-eyehand-only 0)
  (setf *inanimate-object-motions* nil)
  (values))

(defun COUNT-OBJECTS-IN-WORLD ()
  (let ((count 0))
    (dotimes (x *world-diameter*)
      (dotimes (y *world-diameter*)
	(when (get-object x y)
	  (incf count))))
    count))

(defun IDENTIFY-OBJECTS-IN-WORLD ()
  (terpri)
  (loop for y from (1- *world-diameter*) downto 0
	do (loop for x from 0 below *world-diameter*
		 do (let ((object (get-object x y)))
		      (if object
			  (format t "~9A" (identify-object object))
			  (format t "........ "))))
	   (terpri))
  (values))

(defun TEST-TICKER ()
  (loop for counter from 0
	until *inanimate-object-motions*
	do (clock-tick)
	finally (return counter)))

(defun SHOW-SOME-TICKS (&optional (n 100))
  (identify-objects-in-world)
  (loop with initial-length = (length *inanimate-object-motions*)
	until (zerop n)
	do (clock-tick)
	   (let ((new-length (length *inanimate-object-motions*)))
	     (unless (= initial-length new-length)
	       (setf initial-length new-length)
	       (decf n)
	       (identify-objects-in-world))))
  (values))

;;;; Random number generator seed saving function
;;;; NEW-INITIAL-RANDOM-STATE

;;; This function writes a new random-state seed
;;; to the disk file "state.dat" .
;;; This seed will be used the next time the world is initialized.

(defun NEW-INITIAL-RANDOM-STATE
  (&optional (filename "state.dat"))
  
  (with-open-file (out-file filename
                            :direction :output
                            :if-exists :supersede)
   (print (make-random-state t) out-file)
   (terpri out-file))
  'new-initial-random-state-written-to-disk)

;;;; Microworld initialization function INIT-WORLD.

;;; This function loads the current random seed from disk and resets
;;; all the world variables appropriately.
;;; The hand is placed at body relative coordinate (1,0) which is
;;; microspace relative coordinate (3,2).
;;; The center of the visual field is oriented at body relative
;;; coordinate (1,1) which is microspace relative coordinate (3,3).
;;; The hand is left open.
;;; The world is recreated from scratch, with the hand, body and two
;;; objects placed in the appropriate places, and the cycle values
;;; (used by clock-tick for randomly moving objects) reset.
;;; Finally, it resets and returns the value of *CLOCK*.

(defun INIT-WORLD (&optional (filename "state.dat"))
;  (with-open-file (in-file filename
;                           :direction :input
;                           :if-does-not-exist :error)
;   (setf *random-state* (read in-file nil nil nil)))
  filename					; Ignored for now.  --- Foner.
  (setf *grip-expiration* -1
        *hp-x* 1
        *hp-y* 0
        *vp-x* 1
        *vp-y* 1
        *hcl* nil
        *hgr* nil
        *world* (make-array (list *world-diameter* *world-diameter*)	 ; &&& Um, why do we bother setting it in a DEFVAR and here?
			    :initial-element nil)
        (get-object (fix+ 2 *body-x*) (fix+ 2 *body-y*)) *body*	    ; E.g., at 3,1.  Why it doesn't do it more directly, I dunno.
        (get-object (fix+ 2 *hp-x*)   (fix+ 2 *hp-y*))   *hand*	    ; E.g., at 3,2.
        (get-object 1 5) *object-1* ; cycle 0 moves right
        (get-cycle *object-1*) 0
        (get-object 5 5) *object-2* ; cycle 1 moves down
        (get-cycle *object-2*) 1
        *clock* 0))

;;;; Saving and restoring the state of the microworld.

(defparameter *EYEHAND-STATE-SCALARS*
	      '(
		*clock*
		*grip-expiration*
		*hp-x*
		*hp-y*
		*vp-x*
		*vp-y*
		*hcl*
		*hgr*))

(defparameter *EYEHAND-STATE-NONSCALARS*
	      '(
		*world*
		*body*
		*hand*
		*object-1*
		*object-2*))

(defparameter *EYEHAND-STATE-NONSCALAR-COPIERS*
	      '(
		(*world*                   safe-copy-world)
		(*body*                    safe-copy-world-object)
		(*hand*                    safe-copy-world-object)
		(*object-1*                safe-copy-world-object)
		(*object-2*                safe-copy-world-object)))

(defvar *EYEHAND-STATE-VARIABLES*
	(append *eyehand-state-scalars*
		*eyehand-state-nonscalars*))

(defun SAFE-COPY-WORLD-OBJECT (o)
  (let ((new (copy-world-object o)))
    (setf (world-object-visual new)  (copy-seq (world-object-visual new))
	  (world-object-tactile new) (copy-seq (world-object-tactile new))
	  (world-object-taste new)   (copy-seq (world-object-taste new)))
    new))

(defun SAFE-COPY-WORLD (w)
  ;; We assume that the array is 2D.  If I wanted to be Genera-specific, I could
  ;; use the SYS:ARRAY-REGISTER-1D declaration to force them into a 1D framework.
  ;; Alternately, if I really cared about generality, I could use a displaced array to do
  ;; the same thing.  But this is a tiny array, and known to be 2D, so what the hell...
  (let* ((old w)
	 (new (make-array (array-dimensions old)
			  :element-type (array-element-type old))))
    (dotimes (x (array-dimension old 0))
      (dotimes (y (array-dimension old 1))
	(let ((elt (aref old x y)))
	  (when elt
	    (setf (aref new x y)
		  (safe-copy-world-object elt))))))
    new))

(defun EYEHAND-COPIER-FROM-SYMBOL (symbol)
  (second (assoc symbol *eyehand-state-nonscalar-copiers*)))

(defun SAFE-COPY-EYEHAND-STATE ()
  (append
    (loop for scalar-symbol in *eyehand-state-scalars*
	  collect (symbol-value scalar-symbol))
    (loop for nonscalar-symbol in *eyehand-state-nonscalars*
	  for copier = (eyehand-copier-from-symbol nonscalar-symbol)
 	  collect (funcall copier (symbol-value nonscalar-symbol)))))

(defun SAFE-RESTORE-EYEHAND-STATE (snapshot)	; This is NOT an entire snapshot, but only the MICROWORLD-STATE part of it!
  (loop for state-symbol in *eyehand-state-variables*
	for item in snapshot
	for item-is-nonscalar? = (member state-symbol *eyehand-state-nonscalars*)
	for restore-fn = (and item-is-nonscalar?
			      (eyehand-copier-from-symbol state-symbol))
	do (set state-symbol
		(if item-is-nonscalar?
		    (funcall restore-fn item)
		    item)))
  (values))

;;; End of file.
