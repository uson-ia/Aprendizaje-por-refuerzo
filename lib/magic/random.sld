;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic random number computations
;;; ================================

(define-library (magic random)
  (export
   ;; from random/procedures.scm
   random-integer random-real choose
   )
  (import (scheme base)
	  (scheme time))
  (include "./random/generators.scm")
  (include "./random/procedures.scm"))
