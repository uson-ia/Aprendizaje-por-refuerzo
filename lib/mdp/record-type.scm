;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Markov Decision Process library
;;; ===============================
;;;
;;; Record type definition

(define-record-type <mdp>
  (make-mdp states transitions actions rewards)
  mdp?
  (states      mdp-states)
  (transitions mdp-transitions)
  (actions     mdp-actions)
  (rewards     mdp-rewards))

(define-record-type <mdp-sim>
  (make-mdp-sim state? actions transition)
  mdp-sim?
  (state?      mdp-sim-state-pred)
  (actions     mdp-sim-actions)
  (transition  mdp-sim-transition))
