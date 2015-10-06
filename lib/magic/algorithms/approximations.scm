;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic algorithms
;;; ===============================
;;;
;;; Approximation algorithms

(define (approximate good-enough? improve initial-guess)
  (define (next-step current-guess)
    (let ((next-guess (improve current-guess)))
      (if (good-enough? current-guess next-guess)
	  current-guess
	  (next-step next-guess))))
  (next-step (initial-guess)))
