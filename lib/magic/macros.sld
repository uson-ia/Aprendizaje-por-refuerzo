;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Magic macros
;;; ===============================

(define-library (magic macros)
  (export
   define-syntax-rule
   )
  (import (scheme base))
  (include "./macros/syntax-rule.scm"))
