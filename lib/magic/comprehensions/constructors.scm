;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic comprehensions
;;; ===============================
;;;
;;; For structure construction

(define (alist-function alist)
  (lambda (key)
    (cond ((assoc key alist) => cdr)
	  (else (error "value not in function's domain" key)))))

(define-syntax dict
  (syntax-rules (=> for in)
    ((dict key-code => val-code for var in lst)
     (map (lambda (var) (cons key-code val-code)) lst))))

(define-syntax branch-clauses
  (syntax-rules (else)
    ((branch-clauses (x ...) (else result))
     result)
    ((branch-clauses (x ...) ((y ...) result) clause1 clause2 ...)
     (if (equal? (list x ...) (list y ...))
	 result
	 (branch-clauses (x ...) clause1 clause2 ...)))))

(define-syntax-rule (mapping (x ...) clause1 clause2 ...)
  (lambda (x ...) (branch-clauses (x ...) clause1 clause2 ...)))
