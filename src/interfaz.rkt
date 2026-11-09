#lang eopl
;Autores: Oliver De Jesus Arboleda Baez 2459684, Maria Fernanda Betancourt Montoya 2459510, Kevin Andres Rosero Romo 2459554

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
