;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic comprehensions
;;; ===============================
;;;
;;; For iteration

(define-syntax iterate
  (syntax-rules (in do)
    ((iterate var in lst do code)
     (for-each (lambda (var) code) lst))))
