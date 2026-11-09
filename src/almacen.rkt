#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562

#|
  TAD almacén: referencias (índices) -> valores.
  Interfaz: initialize-store!, newref!, deref-ref, setref!,
            almacen-tamano, contenido-almacen.
  Primera entrega: las escrituras son definitivas.
  Segunda entrega: aquí se agregan abrir-transaccion!, confirmar! y
  revertir!. Todo acceso al almacén pasa por esta interfaz, así quien
  lo usa no cambia. almacen-tamano da |sigma0|, que servirá para
  descartar las celdas creadas dentro de un bloque.
|#

(require "valores.rkt")
(provide (all-defined-out))

(define the-store 'sin-inicializar)

(define initialize-store!
  (lambda () (set! the-store '())))

;; verificar-inicializado : symbol -> #t
(define verificar-inicializado
  (lambda (quien)
    (if (list? the-store)
        #t
        (eopl:error quien "el almacén no está inicializado"))))

;; almacen-tamano : () -> entero
(define almacen-tamano
  (lambda ()
    (begin
      (verificar-inicializado 'almacen-tamano)
      (length the-store))))

;; contenido-almacen : () -> listaDeValor (para pruebas e informe)
(define contenido-almacen
  (lambda ()
    (begin
      (verificar-inicializado 'contenido-almacen)
      the-store)))

(define referencia-valida?
  (lambda (r)
    (and (list? the-store)
         (integer? r)
         (>= r 0)
         (< r (length the-store)))))

;; newref! : valor -> entero (índice de la celda nueva)
(define newref!
  (lambda (v)
    (begin
      (verificar-inicializado 'newref!)
      (if (valor? v)
          (let ((n (length the-store)))
            (begin
              (set! the-store (append the-store (list v)))
              n))
          (eopl:error 'newref! "solo se guardan valores del lenguaje: ~s" v)))))

;; deref-ref : entero -> valor
(define deref-ref
  (lambda (r)
    (begin
      (verificar-inicializado 'deref-ref)
      (if (referencia-valida? r)
          (list-ref the-store r)
          (eopl:error 'deref-ref "referencia inválida: ~s" r)))))

;; setref! : entero x valor -> (efecto)
(define setref!
  (lambda (r v)
    (begin
      (verificar-inicializado 'setref!)
      (if (referencia-valida? r)
          (set! the-store (reemplazar-nth the-store r v))
          (eopl:error 'setref! "referencia inválida: ~s" r)))))

;; reemplazar-nth : lista x entero x valor -> lista
(define reemplazar-nth
  (lambda (lista n v)
    (if (= n 0)
        (cons v (cdr lista))
        (cons (car lista) (reemplazar-nth (cdr lista) (- n 1) v)))))