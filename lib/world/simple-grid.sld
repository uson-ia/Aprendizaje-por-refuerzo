;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Simple grid world
;;; ===============================

(define-library (world simple-grid)
  (export
   rotate-left
   rotate-right
   opposite
   gw:states
   go
   gw:transitions
   gw:actions
   gw:rewards
   gridworld
   )
  (import (scheme base)
	  (mdp)
	  (magic algorithms)
	  (magic comprehensions))
  (include "./simple-grid/grid.scm"))
