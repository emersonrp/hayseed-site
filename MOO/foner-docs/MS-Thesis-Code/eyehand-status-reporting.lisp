;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Eyehand -*-

;;;; Status-reporting functions.

(in-package :eyehand)

(defun EYEHAND-SHOW-ITEMS-VERBOSE ()		; This was SHOW-ITEMS in Ramstad's original implementation, and was a macro there.
  (when schema::*show-items-enabled*
    (eyehand-show-items-enabled)))

;;; Make this stuff easy to call from the listener, so I can see what the state of the world is.
(defun SCHEMA::EYEHAND? ()			; Make this easier to call from SCHEMA.
  (let ((*output-stream* t))			; In case it's otherwise bound, during the progress of a run.
    (terpri)
    (eyehand-show-items-enabled))
  (values))

;;; Not guaranteed to work with anything but the exact original set of primitive
;;; items from the Ramstad/Drescher microworld.  This version uses item names
;;; (instead of item numbers), thereby working correctly in row-major item
;;; creation regimes (e.g., the way that DEF-ITEMS-2D does it), and depends upon
;;; SCHEMA::*PRIMITIVE-ITEM-SYMBOL-NAME-TABLE* (created just a few days before
;;; for another reason) to do the lookup.  Note that, as defined in Ramstad's original
;;; coordinate system, HP & VP items went column-major, whereas FOVx items went
;;; row-major (*sigh*).
(defun EYEHAND-SHOW-ITEMS-ENABLED ()
  ;; Note that this still won't protect us if the items get created in a different
  ;; order than expected, even if there are exactly the same number...
  (err-if-deflimits-not-historical
    *hp-diameter*   *vp-diameter*   *vf-diameter*
    *fovf-diameter* *fovb-diameter* *fovl-diameter*
    *fovr-diameter* *fovx-diameter* *text-diameter*
    *taste-diameter*)
  ;; Now actually do some work.
  (macrolet ((sg (name)
	       `(schema::state-unparse
		  (schema::get-item-current-state
		    (gethash ',name
			     schema::*primitive-item-symbol-name-table*))))
	     (sgs (format-string &rest names)
	       `(format *output-stream*
			,format-string
			,@(loop for name in names
				collect `(sg ,name)))))
    (format *output-stream*			; Heading.
	    "hp     vp     vf       fovf  fovb  fovl  fovr  fovx    ~
             tact  body~%")
    (sgs					; Line 1.
      "~A~A~A    ~A~A~A    ~A~A~A~A~A    ~A~A~A~A  ~
       ~A~A~A~A  ~A~A~A~A  ~A~A~A~A  ~A~A~A~A     ~
       ~A     ~A~%"
      hp02 hp12 hp22
      vp02 vp12 vp22
      vf04 vf14 vf24 vf34 vf44
      fovf30 fovf31 fovf32 fovf33
      fovb30 fovb31 fovb32 fovb33
      fovl30 fovl31 fovl32 fovl33
      fovr30 fovr31 fovr32 fovr33
      fovx30 fovx31 fovx32 fovx33
      tactf bodyf)
    (sgs					; Line 2.
      "~A~A~A    ~A~A~A    ~A~A~A~A~A    ~A~A~A~A  ~
       ~A~A~A~A  ~A~A~A~A  ~A~A~A~A  ~A~A~A~A    ~
       ~A ~A   ~A ~A~%"
      hp01 hp11 hp21
      vp01 vp11 vp21
      vf03 vf13 vf23 vf33 vf43
      fovf20 fovf21 fovf22 fovf23
      fovb20 fovb21 fovb22 fovb23
      fovl20 fovl21 fovl22 fovl23
      fovr20 fovr21 fovr22 fovr23
      fovx20 fovx21 fovx22 fovx23
      tactl tactr
      bodyl bodyr)
    (sgs					; Line 3.
      "~A~A~A    ~A~A~A    ~A~A~A~A~A    ~A~A~A~A  ~
       ~A~A~A~A  ~A~A~A~A  ~A~A~A~A  ~A~A~A~A     ~
       ~A     ~A~%"
      hp00 hp10 hp20
      vp00 vp10 vp20
      vf02 vf12 vf22 vf32 vf42
      fovf10 fovf11 fovf12 fovf13
      fovb10 fovb11 fovb12 fovb13
      fovl10 fovl11 fovl12 fovl13
      fovr10 fovr11 fovr12 fovr13
      fovx10 fovx11 fovx12 fovx13
      tactb bodyb)
    (sgs					; Line 4.
      "              ~A~A~A~A~A    ~A~A~A~A  ~
       ~A~A~A~A  ~A~A~A~A  ~A~A~A~A  ~A~A~A~A    "
      vf01 vf11 vf21 vf31 vf41
      fovf00 fovf01 fovf02 fovf03
      fovb00 fovb01 fovb02 fovb03
      fovl00 fovl01 fovl02 fovl03
      fovr00 fovr01 fovr02 fovr03
      fovx00 fovx01 fovx02 fovx03)
    (format *output-stream* "~A  ~A   ~A  ~A~%"
	    "text" "taste" "hcl" "hgr")
    (sgs					; Line 5.
      "              ~A~A~A~A~A                                    ~
       ~A~A~A~A  ~A~A~A~A     ~A    ~A~%"
      vf00 vf10 vf20 vf30 vf40
      text0 text1 text2 text3
      taste0 taste1 taste2 taste3
      hcl hgr)
    (format *output-stream* "~%"))
  (values))

;;; End of file.
