;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic algorithms
;;; ===============================
;;;
;;; Basic algorithms

(define (difference x y)
  (abs (- x y)))

(define (range-aux start end step compare)
  (if (compare start end)
      '()
      (cons start (range-aux (+ start step) end step compare))))

(define range
  (case-lambda
   ((end) (range 0 end))
   ((start end) (range start end 1))
   ((start end step)
    (range-aux start end (if (<= start end) step (- step)) (if (<= start end) >= <=)))))

(define (filter pred lst)
  (cond ((null? lst) '())
	((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
	(else (filter pred (cdr lst)))))

(define (remove pred lst)
  (cond ((null? lst) '())
	((pred (car lst)) (remove pred (cdr lst)))
	(else (cons (car lst) (remove pred (cdr lst))))))
