#lang eopl
(require "sintaxis.rkt" "ambiente.rkt" "almacen.rkt" "valores.rkt")
(provide (all-defined-out))

;;; ==========================================================
;;; 8. Interpretador
;;; ==========================================================

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) n))))
