;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic random number computations
;;; ================================
;;;
;;; Generating random numbers

(define (make-linear-congruential-generator m a c)
  (define prev (current-jiffy))
  (lambda ()
    (define next (remainder (+ (* a prev) c) m))
    (set! prev next)
    next))

(define the-knuth
  (make-linear-congruential-generator 18446744073709551616 ;2^64
				      6364136223846793005
				      1442695040888963407))
