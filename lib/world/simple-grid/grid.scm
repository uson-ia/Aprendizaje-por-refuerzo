;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Simple grid world
;;; ===============================
;;;
;;; Two-dimensional gridworld

(define (rotate-left action)
  (case action
    ((up)    'left)
    ((down)  'right)
    ((left)  'down)
    ((right) 'up)))

(define (rotate-right action)
  (case action
    ((up)    'right)
    ((down)  'left)
    ((left)  'up)
    ((right) 'down)))

(define (opposite action)
  (case action
    ((up)    'down)
    ((down)  'up)
    ((left)  'right)
    ((right) 'left)))

(define (go state action states)
  (define x (car state))
  (define y (cadr state))
  (let ((next (case action
		((up)    (list x (+ y 1))) ((down)  (list x (- y 1)))
		((left)  (list (- x 1) y)) ((right) (list (+ x 1) y)))))
    (define x* (car next))
    (define y* (cadr next))
    (if (or (not (member next states))
	   (< x* 0) (> x* 3) (< y* 0) (> y* 2))
	state
	next)))

(define-record-type <grid>
  (grid: width height cells cell-lst)
  grid?
  (width    grid-width)
  (height   grid-height)
  (cells    grid-cells)
  (cell-lst grid-cell-lst))

(define (grid width height . cell-lst)
  (define (vectorize lst)
    (if (null? lst)
	#()
	(vector-append (vectorize (list-tail lst width))
		       (list->vector
			(list-head lst width)))))
  (grid: width height (vectorize cell-lst) cell-lst))

(define (show-grid grd)
  (define lst   (grid-cell-lst grd))
  (define width (grid-width grd))
  (let loop ((xs lst)
	     (i 0))
    (unless (null? xs)
	    (when (zero? (remainder i width))
		  (display "\n"))
	    (display (car xs))
	    (display " ")
	    (loop (cdr xs) (+ i 1))))
  (display "\n")
  '~~~GRID~~~)

(define (grid-ref grd x y)
  (vector-ref (grid-cells grd)
	      (+ x (* y (grid-width grd)))))

(define (coordinates width height)
  (apply append (map (lambda (x) (map (lambda (y) (list x y)) (range height))) (range width))))

(define (make-simple-grid grd)
  (define coords (coordinates (grid-width grd)
			      (grid-height grd)))
  (define good
    (car (filter (lambda (c)
		   (case (grid-ref grd (car c) (cadr c))
		     ((good) #t)
		     (else #f)))
		 coords)))
  (define hell
    (car (filter (lambda (c)
		   (case (grid-ref grd (car c) (cadr c))
		     ((hell) #t)
		     (else #f)))
		 coords)))
  (define states
    (filter (lambda (c)
	      (case (grid-ref grd (car c) (cadr c))
		((step good hell) #t)
		(else #f)))
	    coords))
  (define transitions
    (mapping (s a s*)
	     ((s a (go s a states))                0.8)
	     ((s a (go s (rotate-left a) states))  0.1)
	     ((s a (go s (rotate-right a) states)) 0.1)
	     (else 0.0)))
  (define actions
    (lambda (state)
      '(up down left right)))
  (define rewards
    (mapping (s a s*)
	     ((s a hell) -1.0)
	     ((s a good) +1.0)
	     (else       -0.04)))
  (make-mdp states transitions actions rewards))
