;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: FLOAT -*-

;;; This file wasdeveloped by Robert Ramstad with help from 
;;; Paul Anagnostopoulous, Digital Equipment Corporation.
;;; It allows for easy optimization of procedures using
;;; short-float arithmetic.

(in-package :float)

(export
 '(float=
   float/=
   float>
   float>=
   float<
   float<=
   float+
   float-
   float*
   float/
   float1+
   float1-))

;;; Use of these macros helps math performance without adding extra
;;; typing - while possibly redundant given the use of declarations
;;; throughout, using these macros insures that the compiler generates
;;; code which uses short-float math and compare operations exclusively.

(defmacro FLOAT= (x y)
  `(= (the short-float ,x) (the short-float ,y)))

(defmacro FLOAT/= (x y)
  `(/= (the short-float ,x) (the short-float ,y)))

(defmacro FLOAT> (x y)
  `(> (the short-float ,x) (the short-float ,y)))

(defmacro FLOAT>= (x y)
  `(>= (the short-float ,x) (the short-float ,y)))

(defmacro FLOAT< (x y)
  `(< (the short-float ,x) (the short-float ,y)))

(defmacro FLOAT<= (x y)
  `(<= (the short-float ,x) (the short-float ,y)))

(defmacro FLOAT+ (x y)
  `(the short-float (+ (the short-float ,x) (the short-float ,y))))

(defmacro FLOAT- (x &rest more-nums)
  `(the short-float (- (the short-float ,x)
                       ,@(mapcar #'(lambda (n) `(the short-float ,n))
                                 more-nums))))
(defmacro FLOAT* (x y)
  `(the short-float (* (the short-float ,x) (the short-float ,y))))

(defmacro FLOAT/ (x y)
  `(the short-float (/ (the short-float ,x) (the short-float ,y))))

(defmacro FLOAT1+ (x)
  `(the short-float (1+ (the short-float ,x))))

(defmacro FLOAT1- (x)
  `(the short-float (1- (the short-float ,x))))

;;; End of file.
