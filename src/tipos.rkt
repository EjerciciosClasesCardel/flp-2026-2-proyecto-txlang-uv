#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2, Nombre3 Codigo3

#|
  Chequeador de tipos (segunda entrega). Mientras no exista, deja pasar todo.
|#

(require "sintaxis.rkt")
(provide (all-defined-out))

(define chequear-programa
  (lambda (pgm) pgm))

(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa
                "el chequeador de tipos llega en la segunda entrega")))