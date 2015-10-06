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

(define-record-type <gridworld>
  (make-gridworld width height grid valid-cell?)
  gridworld?
  (width       grid-width)
  (height      grid-height)
  (grid        grid                  set-grid!)
  (valid-cell? gridworld-valid-cell?))

(define (gridworld->mdp gridworld)
  (make-mdp
   ))
