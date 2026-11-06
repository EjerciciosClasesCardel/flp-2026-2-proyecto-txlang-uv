#lang eopl

(require "sintaxis.rkt"
         "ambiente.rkt"
         "almacen.rkt"
         "valores.rkt"
         "tipos.rkt"
         "interprete.rkt")

;; Interpreta un programa TxLang
(define interpretar
  (lambda (texto)
    (let ((arbol (scan&parse texto)))
      (let ((arbol-validado (chequear-programa arbol)))
        (cases programa arbol-validado
          (un-programa (declaraciones expresion)
                       (begin
                         (inicializar-almacen!)
                         (value-of expresion (amb-vacio)))))))))

;; Ejecuta un programa y devuelve su resultado como texto
(define correr
  (lambda (texto)
    (expval->texto (interpretar texto))))

(provide (all-defined-out))
