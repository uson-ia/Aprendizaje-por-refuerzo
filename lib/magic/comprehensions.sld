;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic comprehensions
;;; ===============================

(define-library (magic comprehensions)
  (export
   ;; from comprehensions/reducers.scm
   sum val-max arg-max
   
   ;; from comprehensions/constructors.scm
   dict mapping)
  (import (scheme base)
	  (magic macros))
  (include "./comprehensions/reducers.scm")
  (include "./comprehensions/constructors.scm"))
