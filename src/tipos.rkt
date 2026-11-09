#lang eopl
;Autores: Oliver De Jesus Arboleda Baez 2459684, Maria Fernanda Betancourt Montoya 2459510, Kevin Andres Rosero Romo 2459554

(require "sintaxis.rkt")

;; =========================================================
;; MODULO: TIPOS
;; Funciones de apoyo para el manejo de tipos.


;; =========================================================
;; CHEQUEAR PROGRAMA
;;
;; En esta etapa se deja pasar el programa sin realizar
;; una verificacion estatica completa de tipos

(define chequear-programa
  (lambda (programa)
    programa))


;; =========================================================
;; TIPO DEL PROGRAMA
;;
;; La inferencia o verificacion completa de tipos se deja
;; para la etapa correspondiente del proyecto.

(define tipo-del-programa
  (lambda (programa)
    (eopl:error
     'tipo-del-programa
     "La verificacion de tipos aun no esta implementada en esta entrega.")))


(provide (all-defined-out))
