#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2, Nombre3 Codigo3

(provide (all-defined-out))

;;; ==========================================================
;;; 3. Unparse: del árbol de sintaxis abstracta al texto
;;; ==========================================================

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo) (unparse-expresion cuerpo)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (lit-exp (n) (number->string n)))))
