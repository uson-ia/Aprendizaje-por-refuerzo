;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Mundo para el problema del inventario
;;; =====================================

(define-library (world inventario)
  (export
   make-inventory
   default-inventory
   test-inventory
   )
  (import (scheme base)
	  (scheme write)
	  (mdp)
	  (magic algorithms)
	  (magic comprehensions))
  (include "./inventario/proceso.scm"))
