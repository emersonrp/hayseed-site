;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Schema -*-

;;;; Reading a sequence of animation commands.  This is the MCL side of the protocol,
;;;; and hence must be run in the MCL process, not in Genera.

(in-package :schema)

;;; Assumes only one level of init-command nesting.
(defun SUMMARIZE-ANIMATION-COMMANDS (commands)
  #+Genera (declare (values inits commands))
  (values (count-if     #'consp commands :key #'car)
	  (count-if-not #'consp commands :key #'car)))

(defvar *ANIMATION-COMMANDS* nil)

(defun GET-ALL-ANIMATION-TITLES (commands &optional (opcode :retitle))
  (let* ((inits
	   (loop for command in commands
		 when (consp (car command))
		   append command))
	 (titles
	   (remove opcode inits :test-not #'eql :key #'car))
	 (title-strings
	   (mapcar #'second titles)))
    title-strings))

(defun SUMMARIZE-ANIMATION-TITLES (commands &optional (opcode :retitle))
  (let ((titles (get-all-animation-titles commands opcode)))
    (when titles
      (format t "~2&Parts of this demo:~&")
      (loop for title in titles
	    do (format t "~&  ~A~&" title))))
  (values))

(defun READ-ANIMATION-COMMANDS (pathname &optional
				(verbose t)
				(set-global t))
  (let ((data
	  (with-open-file (stream pathname)
	    (read stream))))
    (when verbose
      (multiple-value-bind (inits commands)
	  (summarize-animation-commands data)
	(format t "~&~D init~:P, ~D command~:P read.~&"
		inits commands)
	(summarize-animation-titles data)))
    (cond (set-global
	   (setf *animation-commands* data)
	   (values))				; Don't need returned values here; probably calling from listener.
	  (t
	   data))))				; No other way to get them but to return them; probably calling from program.

(defun ANIMATION-PROMPT (prompt)
  (format t "~&~A" prompt)
  (read-char)
  (terpri)
  (values))

;;; No support for cal-agent iconography yet.  Note that this nonetheless
;;; supports a superset of the commands that we might actually issue.
;;; ---OPCODES---
(defun EXECUTE-ANIMATION-COMMAND (command)
  (destructuring-bind (opcode x y)
      command
    #-MCL y					; Y is only used in the MCL code.
    (ecase opcode
      (:eye     #+MCL (move-visual-field *world* x y))
      (:hand    #+MCL (move-hand *hand*  *world* x y))
      (:obj1    #+MCL (move-object1 *object1* *world* x y))
      (:obj2    #+MCL (move-object2 *object2* *world* x y))
      (:grasp   #+MCL (change-to-close-hand *hand* *world*))
      (:ungrasp #+MCL (change-to-open-hand  *hand* *world*))
      (:invert1 #+MCL (invert-object1 *object1* *world*))
      (:invert2 #+MCL (invert-object2 *object1* *world*))
      (:retitle #+MCL (set-window-title *world* x))
      (:prompt        (animation-prompt x))
      ))
  (values))

;;; Implements controlled execution of an animation.
(defun ANIMATION-PAUSE (pause)
  (cond ((or (not (numberp pause))
	     (minusp pause))
	 (format t "~&Step? ")
	 (read-char))
	((zerop pause)				; In Genera, (SLEEP 0) inside a loop actually sleeps a little bit!
	 nil)					; I think we're relinquishing for a timeslice or something.  Weird.
	(t
	 (sleep pause))))

(defparameter *DEFAULT-ANIMATION-PAUSE* 1)	; Seconds.

;;; So far, isn't clever about waiting after initializing, etc.
(defun RUN-ANIMATION-COMMANDS (&key (data *animation-commands*)
			       (pause *default-animation-pause*)
			       (verbose t)
			       inits-only)
  (when verbose
    (multiple-value-bind (inits commands)
	(summarize-animation-commands data)
      (format t "~&Running ~D init~:P~:[, ~D command~:P...~;.~]~&"
	      inits inits-only commands)))
  (loop for command in data
	do (cond ((consp (car command))		; Initialization.  Recurse.
		  (run-animation-commands	; Note that we assume that everything inside is an init (for INITS-ONLY), and don't pass it on.
		    :data command		; The "command" is actually a sequence of them.
		    :pause 0			; Run initialization at maximum speed.
		    :verbose nil))		; Don't tell us about the contents of the initialization.
		 (t				; Normal command.
		  (unless inits-only
		    (execute-animation-command command)
		    (animation-pause pause)))))	; Let us see the results for a while.
  (when verbose
    (format t "~&Done.~&"))
  (values))

;;; End of file.
