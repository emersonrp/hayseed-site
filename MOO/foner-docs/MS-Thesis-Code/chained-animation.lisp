;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

(in-package :schema)

;;;; Spitting out a sequence of animation commands.

(defun CHAIN->ACTIONS (indices)			; Goes from indices into the schema array to indices into the action array.
  (mapcar #'(lambda (index)
	      (schema-action-item (get-schema index)))
	  indices))

;;; Return all but the first and last actions.  The first action shouldn't be taken,
;;; because it's in the schema which is our starting point (hence we've already
;;; taken the action to get there), and the last shouldn't be taken, becuase it's
;;; embedded in the schema which is our goal (hence we don't want to take at
;;; action, lest we overshoot the goal).
(defun SEARCH->BEHAVIOR (start end)
  (let ((path (iterative-depth-first-generate-action-sequence start end)))
    (search-path->behavior path)))

;;; NOTE!  This takes the kind of reversed path that the searching code provides.
(defun SEARCH-PATH->BEHAVIOR (search-path)
  (let* ((path (reverse search-path))
	 (actions (chain->actions path)))
    (loop for index in path
	  for action in actions			; This is the action number indicated by the schema we're printing.
	  do (format t "~&~A (action ~D)~&"
		     (schema-print-name (get-schema index))
		     action))
    (cdr (butlast actions))))

;;; +++ This stuff duplicates some of the functionality of a particular microworld.
;;; +++ It is therefore a KLUDGE and is only for the purposes of quickly getting
;;; +++ the demo operational!

;;; +++ NOTE!  This stuff does NOT understand that moving the hand while grasping
;;; an object moves the object.  Though we store an initial position for object-1 and
;;; object-2, we currently never update it.  (Nor, for that matter, do we actually
;;; remember whether the hand is open or closed.)

;;; Each command is a triple of some keyword and either a pair of coordinates or
;;; two NILs, with one exception:  If the car of the triple is itself a list, then that list
;;; is a nested sequence of commands.  The idea is to spit out a funny "command"
;;; like this at the start of the animation, and to have the animation execution
;;; system use that funny first command to set up the picture, before running any
;;; commands.

;;; To make it easy to find the actual opcodes that might be emitted, each routine
;;; that produces some has ---OPCODES--- as a comment.

;;; Stores where things are in our simulation.  The coordinates are the lower-left
;;; (for objects that are bigger than one pixel) of the object, universe-centered.
(defstruct FAKE-EYEHAND
  hand-x hand-y
  obj1-x obj1-y
  obj2-x obj2-y
  vf-x   vf-y
  grasp
  title
  )

;;; Critically dependent upon the order in which actions are defined,
;;; to avoid a general lookup (purely a code simplification, and not a
;;; huge one at that).  The returned symbol tells us what moved, and
;;; hence what needs its position or status updated.
;;; ---OPCODES---
(defun ACTION-NUMBER->SIMULATED-ACTION (world action)
  (ecase action
    (0 (incf    (fake-eyehand-hand-y world)) :hand)  ; HANDF.
    (1 (decf    (fake-eyehand-hand-y world)) :hand)  ; HANDB.
    (2 (incf    (fake-eyehand-hand-x world)) :hand)  ; HANDR.
    (3 (decf    (fake-eyehand-hand-x world)) :hand)  ; HANDL.
    (4 (incf    (fake-eyehand-vf-y   world)) :eye)   ; EYEF.
    (5 (decf    (fake-eyehand-vf-y   world)) :eye)   ; EYEB.
    (6 (incf    (fake-eyehand-vf-x   world)) :eye)   ; EYER.
    (7 (decf    (fake-eyehand-vf-x   world)) :eye)   ; EYEL.
    (8 :grasp   (setf (fake-eyehand-grasp world) t)   :grasp)  ; GRASP.
    (9 :ungrasp (setf (fake-eyehand-grasp world) nil) :ungrasp)	    ; UNGRASP.
    ))

;;; A convenience function, for handcrafting action sequences.
(defun ACTION-NAME->NUMBER (name)
  (let* ((canonical
	   (etypecase name
	     (symbol (intern (symbol-name name) :keyword))
	     (string (intern-up name :keyword))))
	 (action
	   (position canonical '(:handf :handb :handr :handl :eyef :eyeb :eyer :eyel :grasp :ungrasp))))
    (unless action
      (error "Didn't find an action number for ~S." name))
    action))

(defun ACTION-NAMES->NUMBERS (names)
  (mapcar #'action-name->number names))

(defvar *FAKE-WORLD* nil)

;;; This matches the conditions created by EYEHAND::INIT-WORLD.
(defun CREATE-INITIAL-FAKE-WORLD (&optional (title "World"))
  (setf *fake-world*
	(make-fake-eyehand
	  :hand-x 3 :hand-y 2
	  :obj1-x 1 :obj1-y 5
	  :obj2-x 5 :obj2-y 5
	  :vf-x   1 :vf-y   1
	  :grasp  nil
	  :title  title
	  )))

(defun ACTION-NUMBER->EMITTED-COMMAND (world action)
  (let ((affected (action-number->simulated-action world action)))
    (ecase affected
      (:hand (list affected (fake-eyehand-hand-x world) (fake-eyehand-hand-y world)))
      (:eye  (list affected (fake-eyehand-vf-x world)   (fake-eyehand-vf-y world)))
      ((:grasp :ungrasp) (list affected nil nil)))))

(defun ACTION-NUMBERS->EMITTED-COMMANDS (world actions)
  (mapcar #'action-number->emitted-command
	  (circular-list world)
	  actions))

;;; ---OPCODES---
(defun CREATE-INITIAL-ANIMATION-COMMANDS (&optional (default-world *fake-world*))
  `((:hand    ,(fake-eyehand-hand-x default-world) ,(fake-eyehand-hand-y default-world))
    (:obj1    ,(fake-eyehand-obj1-x default-world) ,(fake-eyehand-obj1-y default-world))
    (:obj2    ,(fake-eyehand-obj2-x default-world) ,(fake-eyehand-obj2-y default-world))
    (:eye     ,(fake-eyehand-vf-x   default-world) ,(fake-eyehand-vf-y   default-world))
    (,(if      (fake-eyehand-grasp  default-world) :grasp :ungrasp) nil nil)
    (:retitle ,(fake-eyehand-title default-world) nil)
    ))

(defun CREATE-INITIAL-ANIMATION-COMMANDS-WITH-PROMPT (prompt &optional (default-world *fake-world*))
  (append (create-initial-animation-commands default-world)
	  `((:prompt ,prompt nil))))

;;; Used when an object gets grasped.
;;; ---OPCODES---
(defun INVERT-OBJECT (number)
  (ecase number
    (1 `(:invert1 nil nil))
    (2 `(:invert2 nil nil))))

(defparameter *DEFAULT-ANIMATION-PATHNAME* "host:users:users:foner:cecile:animation.data")

(defun WRITE-ACTION-DATA (data pathname verbose)
  (with-open-file (stream pathname :direction :output)
    (write data :stream stream))
  (when verbose
    (multiple-value-bind (inits commands)
	(summarize-animation-commands data)
      (format t "~&~D init~:P, ~D command~:P written.~&"
	      inits commands)))
  (values))

(defun WRITE-ACTIONS-FROM-NUMBERS (world actions &key
				   (pathname *default-animation-pathname*)
				   (initial-commands (create-initial-animation-commands))
				   (verbose t))
  (let ((data (append (list initial-commands)
		      (action-numbers->emitted-commands world actions))))
    (write-action-data data pathname verbose)))

; (defun WA-TEST (&optional (actions '(0 1 2 3 4 5 6 7 8 9)))
;   (write-actions *fake-world* actions))

;;;; Simulating random behavior.

;;; For use much later, in animating a real run.  This is much easier than
;;; passing the thing down through all the layers of callers.
(defvar *FAKE-COLLISION-NEVER-HAPPENS* nil)

;;; If the given coordinates match that of any ot object, returns T.
;;; Conses.  Who cares.
(defun FAKE-COLLISION? (x y &optional (world *fake-world*))
  (unless *fake-collision-never-happens*
    (let ((x-accessors '(fake-eyehand-hand-x fake-eyehand-obj1-x fake-eyehand-obj2-x))
	  (y-accessors '(fake-eyehand-hand-y fake-eyehand-obj1-y fake-eyehand-obj2-y)))
      (flet ((get-points (accessors)
	       (loop for accessor in accessors
		     collect (funcall accessor world))))
	(let ((x-points (get-points x-accessors))
	      (y-points (get-points y-accessors)))
	  (push 3 x-points)			; Body position isn't explicitly represented in fake-world.
	  (push 1 y-points)
	  (loop for x-point in x-points
		for y-point in y-points
		thereis (and (= x x-point)
			     (= y y-point))))))))

;;; Hardcoded correspondences between actions & directions.
(defun FAKE-ACTION->DIRECTION (action)
  (ecase action
    ((0 4) :up)
    ((1 5) :down)
    ((2 6) :right)
    ((3 7) :left)))

;;; Computes where the new location will be.
(defun FAKE-NEW-LOCATION (direction x y)
  #+Genera (declare (values new-x new-y))
  (ecase direction
    (:up    (values x      (1+ y)))
    (:down  (values x      (1- y)))
    (:left  (values (1- x) y))
    (:right (values (1+ x) y))))

;;; Crucially dependent on mappings from action numbers to actions.
;;; Only cares about hand motions, since those are the only motions
;;; we can possibly make that can collide with something.
(defun ACTION->FAKE-MOTION (action)
  (case action					; Deliberately not ecase!
    (0 :up)
    (1 :down)
    (2 :right)
    (3 :left)))

;;; Some action that we can perform (e.g., not an independent event like an object
;;; moving around).
(defun FAKE-RANDOM-ACTION ()
  (random 10))

;;; Dependent upon the lower-left convention for big objects (e.g., VF).
;;; A legal value must be low <= value <= high.  Note that these are both inclusive.
(defun FAKE-MOTION-LIMITS (what)
  #+Genera (declare (values low-x low-y high-x high-y))
  (ecase what
    (:eye  (values 0 0 2 2))
    (:hand (values 2 2 4 4))))

(defun FAKE-MOTION-VALID? (action motion-type x-accessor y-accessor
			   &optional (world *fake-world*))
  (multiple-value-bind (new-x new-y)
      (fake-new-location (fake-action->direction action)
			 (funcall x-accessor world)
			 (funcall y-accessor world))
    (multiple-value-bind (low-x low-y high-x high-y)
	(fake-motion-limits motion-type)
      (and (<= low-x new-x high-x)
	   (<= low-y new-y high-y)
	   (not (fake-collision? new-x new-y world))))))  

(defun EMIT-FAKE-MOTION-IF-VALID (action motion-type x-accessor y-accessor
				  &optional (world *fake-world*))
  (when (fake-motion-valid? action motion-type x-accessor y-accessor world)
    ;; No constraints violated, so actually perform the action and emit the opcode.
    (action-number->emitted-command world action)))

;;; Computes a fake action, checks to see if it is invalid (e.g., past a limit point or in
;;; collision), and emits either the appropriate opcode or (if invalid) NIL.
;;; ---OPCODES---
(defun IMPLEMENT-FAKE-RANDOM-ACTION (&optional (world *fake-world*))
  (let ((action (fake-random-action)))
    (implement-fake-action action world)))

(defun IMPLEMENT-FAKE-ACTION (action &optional (world *fake-world*))
  (ecase action
    ((0 1 2 3)					; Hand motions.
     (emit-fake-motion-if-valid
       action :hand #'fake-eyehand-hand-x #'fake-eyehand-hand-y world))
    ((4 5 6 7)					; Eye motions.
     (emit-fake-motion-if-valid
       action :eye  #'fake-eyehand-vf-x   #'fake-eyehand-vf-y   world))
    ((8 9)					; Grasping & ungrasping.
     (action-number->emitted-command world action))))

;;; HOW-MANY is how many actions we'll generate, not how many tries we'll make
;;; (since not every attempt leads to an action, because of invalid actions).
(defun GENERATE-FAKE-RANDOM-ACTIONS (how-many &optional (world *fake-world*))
  (loop with counter = 0
	until (= counter how-many)
	for action = (implement-fake-random-action world)
	when action
	  do (incf counter) and
	    collect action))

;;; Starts with the world initialized to the default.
;;; [Calling this with HOW-MANY equal to 2000 takes about 9 seconds.]
(defun WRITE-FAKE-RANDOM-ACTIONS (how-many &key
				  (pathname *default-animation-pathname*)
				  (verbose t)
				  (title "Random Behavior"))
  (create-initial-fake-world title)		; This has *FAKE-WORLD* hardwired in, hence there's no point having it passed in from above.
  (let* ((initial-commands			; Compute this _before_ we bash the state of the world.
	   (create-initial-animation-commands-with-prompt "Show random behavior? "))
	 (random-action-opcodes			; Now do some actions.
	   (generate-fake-random-actions how-many))
	 (data					; Now set up the data stream for the animation.
	   (append (list initial-commands)
		   random-action-opcodes)))
    (write-action-data data pathname verbose)))

;;;; Animating what happened on a particular run.

;;; Note that we don't record when we moved OBJECT-1 so we might smash the
;;; hand through it (we can never land on top of OBJECT-2 with unmoving objects &
;;; initial setup).  So we move OBJECT-1 a little at the start to avoid this.  Whadda
;;; kludge.

;;; Someone else had better initialize the world for us.
(defun GENERATE-OPCODES-FROM-RUN (&key snapshot-spec
				  (world *fake-world*))
  (let ((*fake-collision-never-happens* t)
	(action-numbers
	  (reverse (named-meter-from-snapshot '*actions-this-run* snapshot-spec))))
    (loop for action-number in action-numbers
	  for opcode = (implement-fake-action action-number world)
	  when opcode
	    collect opcode)))

(defun WRITE-ACTIONS-FROM-RUN (&key snapshot-spec
			       (pathname *default-animation-pathname*)
			       (verbose t)
			       title)
  (unless title
    (setf title
	  (string-trim '(#\.) (comment-info-from-snapshot snapshot-spec))))
  (create-initial-fake-world title)
;   (decf (fake-eyehand-obj1-x *fake-world*))	; KLUDGE:  Push the light blue object left one square, so we can't possibly grasp it.
  (let* ((initial-commands			; Compute this _before_ we bash the state of the world.
	   (create-initial-animation-commands-with-prompt "Show actions taken during run? "))
	 (run-action-opcodes			; Now do some actions.
	   (generate-opcodes-from-run
	     :snapshot-spec snapshot-spec))
	 (data					; Now set up the data stream for the animation.
	   (append (list initial-commands)
		   run-action-opcodes)))
    (write-action-data data pathname verbose)))

;;;; Handcrafting short sequences.

;;; Hey, this shows that the 1416->2599 path is just completely bogus!  It doesn't
;;; even leave the eye in a legal position!  What the hell is going on???
; (defun MOVIE-FOVEA-TO-HAND ()
;   (create-initial-fake-world "Fovea to hand")	; This has *FAKE-WORLD* hardwired in, hence there's no point having it passed in from above.
;   (setf (fake-eyehand-vf-x *fake-world*) 0)
;   (setf (fake-eyehand-vf-y *fake-world*) 0)
;   (let* ((initial-commands			; Compute this _before_ we bash the state of the world.
;          (create-initial-animation-commands))
;        (action-opcodes			; Now do some actions.
;          (action-numbers->emitted-commands
;            *fake-world*
;            '(4 6 4 6 6)))
;        (data					; Now set up the data stream for the animation.
;          (append (list initial-commands)
;                  action-opcodes)))
;     data))

(defun GENERALIZED-MOVIE (title vf-x vf-y hand-x hand-y &key	; The X & Y coords are optional, in which case we use the defaults.
			  names numbers
			  (include-prompt t)
			  special-prompt)
  (assert (or names numbers))
  (assert (not (and names numbers)))
  (create-initial-fake-world title)
  (when vf-x   (setf (fake-eyehand-vf-x *fake-world*) vf-x))
  (when vf-y   (setf (fake-eyehand-vf-y *fake-world*) vf-y))
  (when hand-x (setf (fake-eyehand-hand-x *fake-world*) hand-x))
  (when hand-y (setf (fake-eyehand-hand-y *fake-world*) hand-y))
  (let* ((initial-commands
	   (cond (include-prompt
		  (create-initial-animation-commands-with-prompt
		    (or special-prompt
			(format nil "Run ~S? " title))))
		 (t
		  (create-initial-animation-commands))))
	 (action-opcodes
	   (action-numbers->emitted-commands
	     *fake-world*
	     (if names
		 (action-names->numbers names)
		 numbers)))		   
	 (data
	   (append (list initial-commands)
		   action-opcodes)))
    data))
  
;;; Call this when you want to specify exactly how the movie will run, piece by piece.
(defun WRITE-GENERALIZED-MOVIE-FROM-SPECS (title vf-x vf-y hand-x hand-y &key names numbers
					   (include-prompt t)
					   special-prompt
					   (verbose t)
					   (pathname *default-animation-pathname*))
  (let ((data (generalized-movie
		title vf-x vf-y hand-x hand-y
		:include-prompt include-prompt
		:special-prompt special-prompt
		:names names
		:numbers numbers)))
    (write-action-data data pathname verbose)))

;;; Call this when you have a function that generates the movie, and just want to write it out.
(defun WRITE-GENERALIZED-MOVIE (script &key
				script-args
				(verbose t)
				(pathname *default-animation-pathname*))
  (let ((data (apply script script-args)))
    (write-action-data data pathname verbose)))

;;; +++ Actual movies.

(defun MOVIE--HAND-TO-FOVEA-MOVING-BOTH (&key (include-prompt t)
					 special-prompt)
  (generalized-movie
    "Fovea to hand, coordinated"
    0 0 4 4
    :include-prompt include-prompt
    :special-prompt special-prompt
    :names '(eyer eyef handl handb)))

(defun MOVIE--HAND-TO-FOVEA-MOVING-UNCOORDINATED (&key (include-prompt t)
						  special-prompt)
  (generalized-movie
    "Fovea to hand, uncoordinated"
    0 0 4 4
    :include-prompt include-prompt
    :special-prompt special-prompt
    :names '(eyer eyer eyel handb eyef handf eyef eyeb handl handb)))


;;; These put something out of bounds no matter where I put the hand & eye.
;;; Obviously, the entire chaining approach is fucked as it stands, but I surely
;;; don't have time to fix that now.
(defun MOVIE--434-TO-638-SHORT (&key (include-prompt t)
				special-prompt)
  (generalized-movie
    "434 to 638, short"
    2 1 4 3
    :include-prompt include-prompt
    :special-prompt special-prompt
    :numbers '(7 5 0 1 0 1)))

(defun MOVIE--434-TO-638-MEDIUM (&key (include-prompt t)
				 special-prompt)
  (generalized-movie
    "434 to 638, medium"
    2 1 4 3
    :include-prompt include-prompt
    :special-prompt special-prompt
    :numbers '(4 7 6 7 6 7 7 5 5 6 7 6 4 6 7 6 7 6 7 7 6 6 7 7 6 4 7 0 1 1)))

;;; End of file.
