;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: FIX -*-

;;; This file was developed by Robert Ramstad with help from 
;;; Paul Anagnostopoulous, Digital Equipment Corporation.
;;; It allows for easy optimization of procedures using fixnum
;;; arithmetic.

(in-package :fix)

(export
 '(fix=
   fix/=
   fix>
   fix>=
   fix<
   fix<=
   fix+
   fix-
   fix*
   fix/
   fix1+
   fix1-
   fixplusp
   fixminusp
   fixrsh))

;;; Use of these macros helps math performance without adding extra
;;; typing - while possibly redundant given the use of declarations
;;; throughout, using these macros insures that the compiler generates
;;; code which uses fixnum math and compare operations exclusively.

(defmacro FIX= (x y)
  `(= (the fixnum ,x) (the fixnum ,y)))

(defmacro FIX/= (x y)
  `(/= (the fixnum ,x) (the fixnum ,y)))

(defmacro FIX> (x y)
  `(> (the fixnum ,x) (the fixnum ,y)))

(defmacro FIX>= (x y)
  `(>= (the fixnum ,x) (the fixnum ,y)))

(defmacro FIX< (x y)
  `(< (the fixnum ,x) (the fixnum ,y)))

(defmacro FIX<= (x y)
  `(<= (the fixnum ,x) (the fixnum ,y)))

(defmacro FIX+ (x y)
  `(the fixnum (+ (the fixnum ,x) (the fixnum ,y))))

(defmacro FIX- (x &rest more-nums)
  `(the fixnum (- (the fixnum ,x) ,@(mapcar #'(lambda (n) `(the fixnum ,n))
                                            more-nums))))
(defmacro FIX* (x y)
  `(the fixnum (* (the fixnum ,x) (the fixnum ,y))))

(defmacro FIX/ (x y)
  `(the fixnum (/ (the fixnum ,x) (the fixnum ,y))))

(defmacro FIX1+ (x)
  `(the fixnum (1+ (the fixnum ,x))))

(defmacro FIX1- (x)
  `(the fixnum (1- (the fixnum ,x))))

(defmacro FIXPLUSP (x)
  `(plusp (the fixnum ,x)))

(defmacro FIXMINUSP (x)
  `(minusp (the fixnum ,x)))

(defmacro FIXRSH (x)
  `(the fixnum (ash (the fixnum ,x) -1)))

;;; End of file.
