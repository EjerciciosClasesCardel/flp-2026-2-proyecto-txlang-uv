#lang eopl
(require "sintaxis.rkt" "valores.rkt" "tipos.rkt" "interprete.rkt")
(provide (all-defined-out))


;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (cases programa pgm
          (un-programa (cuerpo) (value-of cuerpo 'ambiente-vacio)))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))
