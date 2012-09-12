;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Hamsterdam -*-

;;;; The linkage from Harlequin LispWorks to the TCP socket that talks to a
;;;; specially-modified version of Hamsterdam.

;;; Note that only the really LispWorks-specific forms are conditionalized.
;;; I could avoid conditionalizing any of them by making the inclusion of the
;;; entire file in the sysdcl conditional upon which platform is in use, but
;;; then it'll be hard to distribute the system (e.g., to the UNIX filesystem
;;; via SCT:EXPORT-SYSTEM et al), and so forth.

(in-package :hamsterdam)

;;;; Linkage to the required C code.

;;; This should give a warning that "the following new foreign symbols are
;;; unresolved:  lisp_parse_turn ... etc".  It can apparently be safely ignored.

;;; &&& This needs to turn into the appropriate loading-form in the sysdcl
;;; &&& instead, both so the absolute pathname here is removed, and so we
;;; &&& can track it in the sysdcl.  However, that can't happen until I look
;;; &&& up the HCL syntax for how to do that.
#+LispWorks
(eval-when (load)
  (ffi:read-foreign-modules "Alive/ServerExamples/lisp-end.o"))

;;;; Foreign interfaces.

#-LispWorks
(defmacro-definer DEF-LISPWORKS-FOREIGN-FUNCTION ((lisp-fn &rest ignore)
						  &rest ignore)
  `(defun ,lisp-fn (&rest ignore)
     (unimplemented "This is the Lisp side of a Harlequin LispWorks foreign-function call.~&~
                     If you are seeing this error, then something is trying to call it, even~&~
                     though the current platform is not Harlequin LispWorks.")))

#+LispWorks
(defmacro-definer DEF-LISPWORKS-FOREIGN-FUNCTION (&body definition)
  `(foreign:define-foreign-function ,@definition))

(def-lispworks-foreign-function (open-socket "ALIVE_open_socket")
				()
  :result-type :uinteger			; Success:  0.  Failure:  1.
  :language :ansi-c)

(def-lispworks-foreign-function (close-socket "ALIVE_close_socket")
				()
  :result-type :integer				; Success:  0.  Failure:  -1 (errno contains failure code).
  :language :ansi-c)

(def-lispworks-foreign-function (write-socket "ALIVE_write_socket")
				((string :simple-string))
  :result-type :integer				; Success:  number of bytes written.  Failure:  -1 (errno contains failure code).
  :language :ansi-c)

(def-lispworks-foreign-function (read-socket "ALIVE_read_socket")
				((string :simple-string))
  :result-type :integer				; Success:  number of bytes read.  Failure:  -1 (errno contains failure code).
  :language :ansi-c)

;;;; Basic communication with the socket.

(defparameter *BUFFER-NULL-CHARACTER* #\Space)

(defun STUFF-BUFFER (buffer format-string &rest format-args)
  (let ((result (apply #'format nil format-string format-args)))
    (when (> (length result) (length buffer))
      (error "The result ~S~&(of length ~D) won't fit in the buffer, which is of length ~D."
	     result (length result) (length buffer)))
    (fill buffer *buffer-null-character*) ; Clear out any trailing gubbish.
    (replace buffer result)))		; Stick in the new command.

(defparameter *SOCKET-BUFFER-LENGTH* 256)

(defun MAKE-SOCKET-BUFFER ()
  (make-string *socket-buffer-length* :initial-element #\Space))

(defmacro WITH-SOCKET (&body body)
  `(unwind-protect
       (progn
	 (open-socket)
	 ,@body)
     (close-socket)))

(defparameter *GRACEFUL-TERMINATION-OPCODE* "+++TEARDOWN+++")

(defun STUFF-BUFFER-AND-WRITE (buffer format-string &rest format-args)
  (apply #'stuff-buffer buffer format-string format-args)
  (write-socket buffer))

(defmacro WITH-SOCKET-GRACEFULLY-TERMINATED (&body body)
  `(with-socket
     (unwind-protect
	 (progn
	   ,@body)
       (let ((command (make-socket-buffer)))
	 (stuff-buffer-and-write command "~A" *graceful-termination-opcode*)))))

(defmacro WITH-BUFFERS-AND-SOCKET ((in out) &body body)
  `(let ((,out (make-socket-buffer))	; Strictly speaking, these could be the same buffer, ...
	 (,in  (make-socket-buffer)))	; ... but this makes debugging easier.
     (with-socket-gracefully-terminated
       ,@body)))

;;;; Test command loops and exercisers.

(defun COMMAND-LOOP ()
  (with-buffers-and-socket (in out)
    (let ((command nil))
      (catch 'done
	(loop do (format t "Command? ")
		 (setf command (read-line))
		 (when (string-equal command ".")
		   (throw 'done nil))
		 (stuff-buffer-and-write out "~A" command)
		 (read-socket in)
		 (format t "~&Sensor echo:  ~S~&" in)))))
  (values))

(defun SEND-COMMAND-IGNORING-ECHO (verbose in out format-string &rest format-args)
  (when verbose
    (apply #'format t format-string format-args)
    (terpri))
  (apply #'stuff-buffer-and-write out format-string format-args)
  (read-socket in)
  (values))

(defun SPIN-THE-PUPPET (theta-low theta-high &optional (verbose t))
  (with-buffers-and-socket (in out)
    (loop for apxsecs from 1 to 30
	  do (loop for theta from theta-low below theta-high by (/ (- theta-high theta-low) 10)
		   do (send-command-ignoring-echo
		       verbose in out "STAND ~F 1 ~F 1.5"
		       apxsecs theta)
		      (send-command-ignoring-echo
		       verbose in out "STAND ~F 1 0 1.5"
		       apxsecs))))
  (values))

;;;; Interfaces from basic actions to the communications substrate.

;;; All of these take output (for outgoing commands) and input (from sensor updates) buffers,
;;; and also whatever parameters the individual command requires.  They also take an optional
;;; debugging arg, which prints out what they're doing.  None of them return any value.

;;; +++ Internal stuff.
(defun CREATE-AND-MAYBE-ECHO-COMMAND (verbose format-string &rest format-args)
  (let ((command (apply #'format nil format-string format-args)))
    (when verbose
      (format t "~A~&" command))
    command))

(defparameter *SHOW-SENSOR-ECHO* t)

(defun COMMAND-AND-SENSE (in out verbose format-string &rest format-args)
  (let ((command (apply #'create-and-maybe-echo-command verbose format-string format-args)))
    (stuff-buffer-and-write out "~A" command)
    (let* ((buffer-length (read-socket in)) ; This should probably always be 256 or whatever the length of the buffer is...
	   (real-length (or (position #\Null in)
			    buffer-length))) ; In case it was totally stuffed, with no room for null termination (at least, we _hope_ that's why!...)
      (when *show-sensor-echo*
        (format t "~&Sensor echo:~&~A~&" 
		(subseq in 0 real-length)))))
  (values))

(defun CONTYPE->ENUM (contype)
  (ecase contype
    (:pelvis 0)
    (:head   1)
    (:lhand  2)
    (:rhand  3)
    (:lfoot  4)
    (:rfoot  5)))

;;; +++ External stuff.
(defun NOOP (in out verbose)
  (command-and-sense in out verbose "NOOP"))

(defun STAND (in out time-to-completion desired-theta velocity &optional verbose)
  (command-and-sense in out verbose "STAND ~D 1 ~D ~D" time-to-completion desired-theta velocity))

(defun SIT (in out velocity &optional verbose)
  (command-and-sense in out verbose "SIT ~D" velocity))

(defun SQUAT (in out velocity &optional verbose)
  (command-and-sense in out verbose "SQUAT ~D" velocity))

(defun TURN (in out x y z angular-velocity &optional verbose)
  (command-and-sense in out verbose "TURN ~D ~D ~D ~D" x y z angular-velocity))

(defun WAVE (in out contype velocity &optional (how-many-times 1) verbose)
  (command-and-sense in out verbose "WAVE ~D ~D ~D" how-many-times velocity (contype->enum contype)))

(defun POINT (in out x y z velocity &optional (how-many-times 1) verbose)
  (command-and-sense in out verbose "POINT ~D ~D ~D ~D ~D" x y z velocity how-many-times))

(defun WALK-TO-SPOT (in out x y z spin-duration velocity &optional verbose)
  (command-and-sense in out verbose "WALK_TO_SPOT ~D ~D ~D ~D ~D" x y z spin-duration velocity))

(defun JUMP (in out drop-time leap-time how-high &optional verbose)
  (command-and-sense in out verbose "JUMP ~D ~D ~D" drop-time leap-time how-high))

(defun GRIN (in out &optional verbose)
  (command-and-sense in out verbose "GRIN"))

(defun SAD (in out &optional verbose)
  (command-and-sense in out verbose "SAD"))

;;; +++ A large test of the above.
(defun TEST-COMMANDS (&optional (verbose t))
  (with-buffers-and-socket (in out)
    (grin  in out verbose)
    (stand in out 10 10 1.5 verbose)
    (sit   in out 15 verbose)
    (stand in out 1 0 1 verbose)	; For some weird reason, SQUAT right after SIT takes practically forever!
    (squat in out 50 verbose)
    (turn  in out -100 0 -150 1.5 verbose)
    (wave  in out :lhand 15 3 verbose)
    (point in out 0 150 -150 15 1 verbose)
    (jump  in out 100 100 30 verbose)
    (sad   in out verbose)
    (sleep 2)				; So we can see the frown.
    (grin  in out verbose))
  (values))

(defun TEST-WALKING (&optional (verbose t))
  (with-buffers-and-socket (in out)
    (loop with done? = nil
	  until done?
	  for coords = (progn (format t "~&Walk to where? (x y z dur vel) ")
			      (read))	; Yuck.
	  do (cond ((consp coords)
		    (destructuring-bind (x y z dur vel) coords
		       (walk-to-spot in out x y z dur vel verbose)))
		   (t
		    (format t "~&Exiting...~&")
		    (setf done? t))))))

;;; End of file.
