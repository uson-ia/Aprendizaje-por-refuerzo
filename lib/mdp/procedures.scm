;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Markov Decision Process library
;;; ===============================
;;;
;;; MDP procedures

(define (state-action-utility mdp discount utility state action)
  (define probability (mdp-transitions mdp))
  (define states      (mdp-states mdp))
  (define reward      (mdp-rewards mdp))
  (sum (* (probability state action next-state)
	  (+ (reward state action next-state)
	     (* discount (utility next-state))))
       for next-state in states))

(define (bellman-utility mdp discount utility state)
  (define actions (mdp-actions mdp))
  (val-max (state-action-utility mdp discount utility state action)
	   for action in (actions state)))

(define (utility->policy mdp discount utility)
  (define actions (mdp-actions mdp))
  (define states  (mdp-states mdp))
  (make-policy state => (arg-max (state-action-utility mdp discount utility state action)
				 for action in (actions state))
	       for state in states))

(define (evaluate-policy mdp discount policy tolerance)
  (define states (mdp-states mdp))
  (approximate (lambda (old-util new-util)
		 (< (val-max (difference (old-util state) (new-util state))
			     for state in states)
		    tolerance))
	       (lambda (old-util)
		 (make-utility state => (state-action-utility mdp discount old-util state (policy state))
			       for state in states))
	       (lambda ()
		 (make-utility state => 0.0 for state in states))))
