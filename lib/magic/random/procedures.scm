;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic random number computations
;;; ================================
;;;
;;; Utility procedures with randomness

(define (random-integer n)
  (remainder (the-knuth) n))

(define (random-real)
  (/ (the-knuth) 18446744073709551616.0))

(define (choose lst)
  (list-ref lst (random-integer (length lst))))
