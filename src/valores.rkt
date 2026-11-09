#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562

#|
  Valores expresados = valores denotados. El ambiente liga identificadores
  directamente a valores; la mutación pasa por las referencias.
  Una referencia es el índice de una celda del almacén.
  El campo amb de la clausura no lleva predicado ambiente? porque
  ambiente.rkt requiere a este módulo y no al revés.
|#

(require "sintaxis.rkt")
(provide (all-defined-out))

(define siempre? (lambda (x) #t))

(define-datatype valor valor?
  (int-val (n integer?))
  (float-val (n number?))
  (str-val (s string?))
  (char-val (c char?))
  (bool-val (b boolean?))
  (unit-val)
  (ref-val (r integer?))
  (proc-val (ids (list-of symbol?)) (cuerpo expresion?) (amb siempre?))
  (registro-val (nombre symbol?) (campos siempre?)))

;; Extractores: reportan error si el valor no es del tipo esperado.
(define valor->bool
  (lambda (v)
    (cases valor v
      (bool-val (b) b)
      (else (eopl:error 'valor->bool "se esperaba un booleano y llegó ~s"
                        (valor->texto v))))))

(define valor->ref
  (lambda (v)
    (cases valor v
      (ref-val (r) r)
      (else (eopl:error 'valor->ref "se esperaba una referencia y llegó ~s"
                        (valor->texto v))))))

(define valor->texto
  (lambda (v)
    (cases valor v
      (int-val (n) (number->string n))
      (float-val (n) (number->string n))
      (str-val (s) s)
      (char-val (c) (list->string (list c)))
      (bool-val (b) (if b "true" "false"))
      (unit-val () "unit")
      (ref-val (r) (string-append "ref(" (number->string r) ")"))
      (proc-val (ids cuerpo amb) "<procedimiento>")
      (registro-val (nombre campos)
        (string-append "<registro " (symbol->string nombre) ">")))))

(define valor->string
  (lambda (v)
    (cases valor v
      (str-val (s) s)
      (else (eopl:error 'valor->string "se esperaba una cadena y llegó ~s"
                        (valor->texto v))))))