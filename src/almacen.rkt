#lang eopl

;; =========================================================
;; MODULO: ALMACEN
;; Administra la memoria del interprete TxLang-UV.


;; Almacen inicial.
;; Cada posicion del vector representa una ubicacion de memoria.

(define almacen
  (make-vector 10 #f))


;; Indica la siguiente posicion disponible.

(define siguiente-posicion 0)


;; =========================================================
;; INICIALIZAR ALMACEN
;; Reinicia la memoria para ejecutar un nuevo programa.

(define inicializar-almacen!
  (lambda ()
    (set! almacen (make-vector 10 #f))
    (set! siguiente-posicion 0)))


;; =========================================================
;; AUMENTAR CAPACIDAD
;; Duplica el espacio disponible cuando se llena el vector.

(define aumentar-capacidad!
  (lambda ()
    (let* ((capacidad-actual (vector-length almacen))
           (nueva-capacidad (* capacidad-actual 2))
           (nuevo-almacen (make-vector nueva-capacidad #f)))

      (letrec
          ((copiar
            (lambda (posicion)
              (if (< posicion capacidad-actual)
                  (begin
                    (vector-set!
                     nuevo-almacen
                     posicion
                     (vector-ref almacen posicion))
                    (copiar (+ posicion 1)))
                  #t))))

        (copiar 0))

      (set! almacen nuevo-almacen))))


;; =========================================================
;; CREAR REFERENCIA
;; Guarda un valor y devuelve su posicion en memoria.

(define nuevo-ref
  (lambda (valor)
    (if (= siguiente-posicion (vector-length almacen))
        (aumentar-capacidad!)
        #t)

    (let ((posicion siguiente-posicion))
      (vector-set! almacen posicion valor)
      (set! siguiente-posicion (+ siguiente-posicion 1))
      posicion)))


;; =========================================================
;; CONSULTAR REFERENCIA
;; Devuelve el valor guardado en una posicion

(define obtener-ref
  (lambda (posicion)
    (if (and (integer? posicion)
             (>= posicion 0)
             (< posicion siguiente-posicion))
        (vector-ref almacen posicion)
        (eopl:error
         'obtener-ref
         "Referencia invalida: ~s"
         posicion))))


;; =========================================================
;; MODIFICAR REFERENCIA
;; Reemplaza el valor guardado en una posicion.

(define actualizar-ref!
  (lambda (posicion valor)
    (if (and (integer? posicion)
             (>= posicion 0)
             (< posicion siguiente-posicion))
        (begin
          (vector-set! almacen posicion valor)
          valor)
        (eopl:error
         'actualizar-ref!
         "Referencia invalida: ~s"
         posicion))))


;; =========================================================
;; CONSULTAR CANTIDAD DE POSICIONES UTILIZADAS

(define cantidad-referencias
  (lambda ()
    siguiente-posicion))


(provide (all-defined-out))