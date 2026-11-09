#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562

(require "sintaxis.rkt" "ambiente.rkt" "almacen.rkt"
         "valores.rkt" "tipos.rkt" "interprete.rkt")
(provide (all-defined-out))

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (initialize-store!)
        (cases programa pgm
          (un-programa (decls cuerpo)
            (value-of cuerpo (empty-env))))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))