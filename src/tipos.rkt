#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562

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