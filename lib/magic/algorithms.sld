;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic algorithms
;;; ===============================

(define-library (magic algorithms)
  (export
   ;; from algorithms/approximations.scm
   approximate

   ;; from algorithms/basic.scm
   difference range filter remove split list-head
   )
  (import (scheme base)
	  (scheme case-lambda))
  (include "./algorithms/approximations.scm")
  (include "./algorithms/basic.scm"))
