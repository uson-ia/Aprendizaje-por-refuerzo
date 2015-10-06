;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Markov Decision Process library
;;; ===============================
;;;
;;; MDP macros

(define-syntax-rule (make-utility x ...)
  (alist-function (dict x ...)))

(define-syntax-rule (make-policy x ...)
  (alist-function (dict x ...)))
