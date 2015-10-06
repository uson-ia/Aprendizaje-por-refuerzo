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
