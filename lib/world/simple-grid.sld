;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Simple grid world
;;; ===============================

(define-library (world simple-grid)
  (export
   make-simple-grid
   grid
   show-grid
   )
  (import (scheme base)
	  (scheme write)
	  (mdp)
	  (magic algorithms)
	  (magic comprehensions))
  (include "./simple-grid/grid.scm"))
