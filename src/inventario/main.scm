;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;; 
;;; basado en el planteamiento del problema de
;;; Julio Waissman.
;;;
;;; Problema del inventario como MDP
;;; ================================

(import (scheme base)
	(scheme write)
	(mdp)
	(world inventario)
	(magic comprehensions))

(define mdp (make-inventory (test-inventory)))
(display "Descripción de probabilidades de transición del mdp del inventario\n\n")

(display ((mdp-transitions mdp) 0 0 0))


(iterate s in (mdp-states mdp) do
	 (iterate a in ((mdp-actions mdp) s) do
		  (iterate s* in (mdp-states mdp) do
			   (when (> ((mdp-transitions mdp) s a s*) 0)
				 (display "p(")
				 (display s)
				 (display ", ")
				 (display a)
				 (display ", ")
				 (display s*)
				 (display ") = ")
				 (display ((mdp-transitions mdp) s a s*))
				 (newline)))))
