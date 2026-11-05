#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2, Nombre3 Codigo3

#|
  TAD almacén: referencias (índices) -> valores.
  Primera entrega: las escrituras son definitivas.
  Segunda entrega: aquí se agregan abrir-transaccion!, confirmar! y
  revertir!, con la pila de transacciones. Por eso todo acceso al almacén
  pasa por newref!, deref-ref y setref!.
|#

(require "valores.rkt")
(provide (all-defined-out))

(define the-store 'sin-inicializar)

(define initialize-store!
  (lambda () (set! the-store '())))

;; newref! : valor -> entero (índice de la celda nueva)
(define newref!
  (lambda (v)
    (let ((n (length the-store)))
      (begin
        (set! the-store (append the-store (list v)))
        n))))

(define referencia-valida?
  (lambda (r)
    (and (integer? r) (>= r 0) (< r (length the-store)))))

;; deref-ref : entero -> valor
(define deref-ref
  (lambda (r)
    (if (referencia-valida? r)
        (list-ref the-store r)
        (eopl:error 'deref-ref "referencia inválida: ~s" r))))

;; setref! : entero x valor -> (efecto)
(define setref!
  (lambda (r v)
    (if (referencia-valida? r)
        (set! the-store (reemplazar-nth the-store r v))
        (eopl:error 'setref! "referencia inválida: ~s" r))))

(define reemplazar-nth
  (lambda (lista n v)
    (if (= n 0)
        (cons v (cdr lista))
        (cons (car lista) (reemplazar-nth (cdr lista) (- n 1) v)))))