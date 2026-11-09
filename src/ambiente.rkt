#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562

#|
  TAD ambiente: liga identificadores a valores.
  amb-rec guarda varias funciones mutuamente recursivas: al buscar una de
  ellas se arma la clausura con el propio ambiente amb-rec, así se ve a sí
  misma y a las demás.
|#

(require "sintaxis.rkt" "valores.rkt")
(provide (all-defined-out))

(define-datatype ambiente ambiente?
  (amb-vacio)
  (amb-extendido (ids (list-of symbol?)) (vals (list-of valor?)) (padre ambiente?))
  (amb-rec (nombres (list-of symbol?))
           (params (list-of (list-of symbol?)))
           (cuerpos (list-of expresion?))
           (padre ambiente?)))

(define empty-env (lambda () (amb-vacio)))

(define extend-env
  (lambda (ids vals amb) (amb-extendido ids vals amb)))

(define extend-env-rec
  (lambda (nombres params cuerpos amb) (amb-rec nombres params cuerpos amb)))

;; posicion : symbol x lista -> entero o #f
(define posicion
  (lambda (x lista)
    (let ciclo ((l lista) (i 0))
      (cond ((null? l) #f)
            ((eqv? (car l) x) i)
            (else (ciclo (cdr l) (+ i 1)))))))

(define apply-env
  (lambda (amb id)
    (cases ambiente amb
      (amb-vacio ()
        (eopl:error 'apply-env "variable no ligada: ~s" id))
      (amb-extendido (ids vals padre)
        (let ((pos (posicion id ids)))
          (if pos (list-ref vals pos) (apply-env padre id))))
      (amb-rec (nombres params cuerpos padre)
        (let ((pos (posicion id nombres)))
          (if pos
              (proc-val (list-ref params pos) (list-ref cuerpos pos) amb)
              (apply-env padre id)))))))