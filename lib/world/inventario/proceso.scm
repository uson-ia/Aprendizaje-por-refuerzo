;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Mundo para el problema del inventario
;;; =====================================
;;;
;;; Proceso de decisión para inventarios

(define-record-type <inventory>
  (inventory: m b d i f dc uc sc bc up dp)
  inventory?
  (m storage-capacity)
  (b backlogging-capacity)
  (d demand-capacity)
  (i items-per-order-capacity)
  (f delivery-time)
  (dc delivery-cost)
  (uc unitary-cost)
  (sc storage-cost)
  (bc backlogging-cost)
  (up unitary-profit)
  (dp demand-probability))

(define (default-inventory)
  (inventory: 10000
	      0
	      1000
	      10000
	      #t
	      0
	      1
	      1
	      0
	      1
	      (lambda (x) 0.000999001)))

(define (test-inventory)
  (inventory: 10 1 5 10 #t 0 1 1 0 10 (lambda (x) 0.090909091)))


(define (make-inventory inv)
  (define states
    (range (- (backlogging-capacity inv))
	   (+ (storage-capacity inv) 1)))
  (define transitions
    (lambda (s a s*)
      (define ap (if (delivery-time inv) 0 1))
      (define sell (+ s a (- s*)))
      (define aux (- (* ap a)
		     (backlogging-capacity inv)))
      (cond ((and (< aux s*)
		(<= s* (+ s a)))
	     ((demand-probability inv) sell))
	    ((= s* aux)
	     (sum ((demand-probability inv) x)
		  for x in (range sell
				  (demand-capacity inv))))
	    (else 0.0))))
  (define actions
    (lambda (state)
      (range (+ 1 (min (+ (storage-capacity inv)
			  (backlogging-capacity inv)
			  (- state))
		       (items-per-order-capacity inv))))))
  (define rewards
    (lambda (s a s*)
      (define ap (if (delivery-time inv) a 0))
      (define cost (+ (* (delivery-cost inv)
			 (if (> a 0) 1 0))
		      (* (unitary-cost inv)
			 a)
		      (* (storage-cost inv)
			 (max 0 (+ s ap (- (backlogging-capacity inv)))))
		      (* (backlogging-cost inv)
			 (max 0 (- (backlogging-capacity inv) s ap)))))
      (define profit (* (unitary-profit inv)
			(+ s ap (- s*))))
      (- profit cost)))
  (make-mdp states transitions actions rewards))
