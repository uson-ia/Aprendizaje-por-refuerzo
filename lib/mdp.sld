;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Markov Decision Process library
;;; ===============================

(define-library (mdp)
  (export
   ;; from mdp/record-type.scm
   <mdp> make-mdp mdp? mdp-states mdp-transitions mdp-actions mdp-rewards

   ;; from mdp/macros.scm
   make-utility make-policy

   ;; from mdp/procedures.scm
   state-action-utility bellman-utility utility->policy evaluate-policy

   ;; from mdp/algorithms.scm
   value-iteration policy-iteration
   q-learning
   )
  (import (scheme base)
	  (magic macros)
	  (magic comprehensions)
	  (magic random)
	  (magic algorithms)
	  (magic table))
  (include "./mdp/record-type.scm")
  (include "./mdp/macros.scm")
  (include "./mdp/procedures.scm")
  (include "./mdp/algorithms.scm"))
