#lang eopl
;Autores: Oliver De Jesus Arboleda Baez 2459684, Maria Fernanda Betancourt Montoya 2459510, Kevin Andres Rosero Romo 2459554

(require "sintaxis.rkt")
(require "ambiente.rkt")


;; =========================================================
;; MODULO: VALORES
;; Define los diferentes valores que puede manejar TxLang-UV.


(define-datatype expval expval?

  ;; Valores numericos: enteros y reales.
  (num-val
   (numero number?))

  ;; Valores booleanos.
  (bool-val
   (booleano boolean?))

  ;; Cadenas de texto.
  (string-val
   (cadena string?))

  ;; Caracteres.
  (char-val
   (caracter char?))

  ;; Procedimientos.
  ;; Guardan sus parametros, cuerpo y ambiente.
  (proc-val
   (parametros list?)
   (cuerpo expresion?)
   (ambiente ambiente?))

  ;; Referencias a posiciones del almacen.
  (ref-val
   (posicion integer?))

  ;; Valor unitario, util cuando una expresion
  ;; no necesita devolver un resultado significativo.
  (unit-val))



;; =========================================================
;; EXTRACTORES DE VALORES
;; Permiten obtener el contenido de cada tipo de valor.


(define expval->num
  (lambda (valor)
    (cases expval valor
      (num-val (numero) numero)
      (else
       (eopl:error 'expval->num
                   "Se esperaba un numero, se recibio: ~s"
                   valor)))))


(define expval->bool
  (lambda (valor)
    (cases expval valor
      (bool-val (booleano) booleano)
      (else
       (eopl:error 'expval->bool
                   "Se esperaba un booleano, se recibio: ~s"
                   valor)))))


(define expval->string
  (lambda (valor)
    (cases expval valor
      (string-val (cadena) cadena)
      (else
       (eopl:error 'expval->string
                   "Se esperaba una cadena, se recibio: ~s"
                   valor)))))


(define expval->char
  (lambda (valor)
    (cases expval valor
      (char-val (caracter) caracter)
      (else
       (eopl:error 'expval->char
                   "Se esperaba un caracter, se recibio: ~s"
                   valor)))))


(define expval->proc
  (lambda (valor)
    (cases expval valor
      (proc-val (parametros cuerpo ambiente)
        (list parametros cuerpo ambiente))
      (else
       (eopl:error 'expval->proc
                   "Se esperaba un procedimiento, se recibio: ~s"
                   valor)))))


(define expval->ref
  (lambda (valor)
    (cases expval valor
      (ref-val (posicion) posicion)
      (else
       (eopl:error 'expval->ref
                   "Se esperaba una referencia, se recibio: ~s"
                   valor)))))



;; =========================================================
;; CONVERSION A TEXTO
;; Convierte un valor expresado en una representacion legible

(define expval->texto
  (lambda (valor)
    (cases expval valor

      (num-val (numero)
        (number->string numero))

      (bool-val (booleano)
        (if booleano "true" "false"))

      (string-val (cadena)
        cadena)

      (char-val (caracter)
        (string caracter))

      (proc-val (parametros cuerpo ambiente)
        "<procedimiento>")

      (ref-val (posicion)
        (string-append
         "<referencia:"
         (number->string posicion)
         ">"))

      (unit-val ()
        "unit"))))


(provide (all-defined-out))
