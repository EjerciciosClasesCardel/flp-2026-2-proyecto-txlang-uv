#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2, Nombre3 Codigo3

(provide (all-defined-out))


;;; ==========================================================
;;; 6. Valores expresados
;;; ==========================================================
;; Un define-datatype con los valores del lenguaje y valor->texto, que es
;; lo que imprime correr.

(define valor->texto
  (lambda (v)
    (cond ((number? v) (number->string v))
          (else (eopl:error 'valor->texto "no sé escribir el valor ~s" v)))))
