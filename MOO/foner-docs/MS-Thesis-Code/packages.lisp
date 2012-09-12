;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Package definitions.

;;; The use of :NICKNAMES and the Genera-specific :PREFIX-NAME below is so the package
;;; name prints out in full, even though I can abbreviate it on typein via the nickname.

(defpackage :FIX)
(defpackage :FLOAT)
(defpackage :SCHEMA)
(defpackage :EYEHAND    (:nicknames :EYE) #+Genera (:prefix-name :EYEHAND))
(defpackage :MUDWORLD   (:nicknames :MUD) #+Genera (:prefix-name :MUDWORLD))
(defpackage :HAMSTERDAM (:nicknames :HAM) #+Genera (:prefix-name :HAMSTERDAM))

(use-package '(:fix :float)  :schema)
(use-package '(:fix :schema) :eyehand)
(use-package '(:schema)      :mudworld)
(use-package '(:schema)      :hamsterdam)

;;;; The code below originated when I was thinking of actually compiling this system
;;;; under MCL.  I never have.  The fix for MCL would be to just make my own
;;;; DEFPACKAGE form in this package that calls the CCL: one appropriately, and do so
;;;; before the DEFPACKAGE forms above.  The solution below is just weird.

; (eval-when (compile load eval)
; (defmacro MAKE-ALL-MY-PACKAGES ()
;   `(progn
;      ,@(loop for (name . nicknames) in '((:fix)
; 					 (:float)
; 					 (:schema)
; 					 (:eyehand :eye)
; 					 (:mudworld :mud)
; 					 (:hamsterdam :ham))
; 	     for nickname-form = (when nicknames
; 				   `((:nicknames ,@nicknames)))
; 	     collect
; 	       #+MCL `(ccl::defpackage ,name (:use :ccl :cl) ,@nickname-form)
; 	       #-MCL `(defpackage ,name ,@nickname-form))))
; 
; (make-all-my-packages)
; )						; EVAL-WHEN.

;;; End of file.
