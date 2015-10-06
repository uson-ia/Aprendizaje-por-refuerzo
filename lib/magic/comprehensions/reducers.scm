;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic comprehensions
;;; ===============================
;;;
;;; For structure->value reductions

(define-syntax sum
  (syntax-rules (for in)
    ((sum code for var in lst)
     (apply + (map (lambda (var) code) lst)))))

(define-syntax val-max
  (syntax-rules (for in)
    ((val-max code for var in lst)
     (apply max (map (lambda (var) code) lst)))))

(define (argmax as vs a* v*)
  (cond ((null? as)
	 a*)
	((> (car vs) v*)
	 (argmax (cdr as) (cdr vs) (car as) (car vs)))
	(else
	 (argmax (cdr as) (cdr vs) a* v*))))

(define-syntax arg-max
  (syntax-rules (for in)
    ((arg-max code for var in lst)
     (let ((as lst)
	   (vs (map (lambda (var) code) lst)))
       (argmax (cdr as) (cdr vs) (car as) (car vs))))))
