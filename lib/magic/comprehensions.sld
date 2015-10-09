;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic comprehensions
;;; ===============================

(define-library (magic comprehensions)
  (export
   ;; from comprehensions/reducers.scm
   sum val-max arg-max all-true?

   ;; from comprehensions/iterators.scm
   iterate
   
   ;; from comprehensions/constructors.scm
   dict mapping alist-function)
  (import (scheme base)
	  (magic macros))
  (include "./comprehensions/reducers.scm")
  (include "./comprehensions/iterators.scm")
  (include "./comprehensions/constructors.scm"))
