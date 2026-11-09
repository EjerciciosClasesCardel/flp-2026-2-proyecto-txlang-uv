#lang eopl
;Autores: Oliver De Jesus Arboleda Baez 2459684, Maria Fernanda Betancourt Montoya 2459510, Kevin Andres Rosero Romo 2459554

(require "sintaxis.rkt")

;; ==========================================================
;; 1. TAD Ambiente
;; El ambiente almacena asociaciones entre identificadores
;; y sus valores.
;;
;; Tiene tres variantes:
;; - Ambiente vacío.
;; - Ambiente extendido.
;; - Ambiente recursivo.

(define-datatype ambiente ambiente?
  (amb-vacio)

  (amb-extendido
   (ids list?)
   (vals list?)
   (amb ambiente?))

  (amb-rec
   (nombres list?)
   (ids-lista list?)
   (cuerpos list?)
   (amb ambiente?)))

;; ==========================================================
;; 2. Resultado de una búsqueda recursiva

;; Se utiliza para evitar una dependencia circular con
;; valores.rkt.
;;
;; Cuando se encuentra una función recursiva, se devuelve
;; esta estructura. El intérprete será el encargado de
;; convertirla en una clausura.

(define-datatype enlace-recursivo enlace-recursivo?
  (enlace-proc-rec
   (ids list?)
   (cuerpo expresion?)
   (amb ambiente?)))

;; ==========================================================
;; 3. Constructor de ambientes

(define extender-amb
  (lambda (ids vals amb)
    (if (= (length ids) (length vals))
        (amb-extendido ids vals amb)
        (eopl:error 'extender-amb
                    "la cantidad de identificadores y valores no coincide"))))

(define extender-amb-rec
  (lambda (nombres ids-lista cuerpos amb)
    (if (and (= (length nombres) (length ids-lista))
             (= (length nombres) (length cuerpos)))
        (amb-rec nombres ids-lista cuerpos amb)
        (eopl:error 'extender-amb-rec
                    "las listas de funciones recursivas no coinciden"))))

;; ==========================================================
;; 4. Búsqueda de identificadores

;; Busca la posición de un identificador en una lista.
;; Devuelve #f si no lo encuentra.

(define posicion
  (lambda (id lista)
    (letrec
        ((buscar
          (lambda (l i)
            (cond
              ((null? l) #f)
              ((eqv? (car l) id) i)
              (else (buscar (cdr l) (+ i 1)))))))
      (buscar lista 0))))

;; ==========================================================
;; 5. Buscar en el ambiente

(define buscar-amb
  (lambda (amb id)
    (cases ambiente amb

      ;; Ambiente vacío: la variable no está definida.
      (amb-vacio ()
        (eopl:error 'buscar-amb
                    "variable no ligada: ~s" id))

      ;; Ambiente extendido: busca primero en las
      ;; asociaciones actuales y después en el anterior.
      (amb-extendido (ids vals amb-anterior)
        (let ((pos (posicion id ids)))
          (if pos
              (list-ref vals pos)
              (buscar-amb amb-anterior id))))

      ;; Ambiente recursivo: busca el nombre de la función.
      (amb-rec (nombres ids-lista cuerpos amb-anterior)
        (let ((pos (posicion id nombres)))
          (if pos
              (enlace-proc-rec
               (list-ref ids-lista pos)
               (list-ref cuerpos pos)
               amb)
              (buscar-amb amb-anterior id)))))))

;; ==========================================================
;; 6. Exportación del módulo

(provide (all-defined-out))
