#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2, ...

#|
  Batería de pruebas del interpretador de TxLang-UV.

  Cada prueba compara el texto que devuelve `correr` con el que se espera, o
  comprueba que un programa se rechace. La primera entrega deja aquí al
  menos 15 programas que cubran todo su alcance; la segunda, al menos 20 que
  cubran el proyecto completo y al menos 5 programas mal tipados con el
  error que el chequeador reporta en cada uno.

  Se ejecuta con:  racket pruebas-txlang.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

;; Corre el programa y, si aborta, devuelve el mensaje precedido de ERROR.
(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define verificar
  (lambda (etiqueta obtenido esperado)
    (if (string=? obtenido esperado)
        (begin (set! pasadas (+ pasadas 1))
               (eopl:printf "  ok    ~a~%" etiqueta))
        (begin (set! fallidas (+ fallidas 1))
               (eopl:printf "  FALLA ~a: se esperaba ~s y llegó ~s~%"
                            etiqueta esperado obtenido)))))

;; El programa devuelve el texto esperado.
(define prueba
  (lambda (etiqueta texto esperado)
    (verificar etiqueta (intentar texto) esperado)))

;; El chequeador asigna al programa el tipo esperado.
(define prueba-tipo
  (lambda (etiqueta texto esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (tipo-del-programa texto))
               esperado)))

;; El programa se rechaza, por el chequeador o por el intérprete.
(define prueba-rechazo
  (lambda (etiqueta texto)
    (let ((r (intentar texto)))
      (if (empieza-con-error? r)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un rechazo y llegó ~s~%" etiqueta r))))))

;; unparse del árbol de un texto vuelve a parsear al mismo árbol.
(define ida-y-vuelta
  (lambda (etiqueta texto)
    (let ((una (unparse (scan&parse texto))))
      (verificar etiqueta (unparse (scan&parse una)) una))))

;;; ==========================================================
;;; Primera entrega: léxico, gramática, unparse y núcleo
;;; ==========================================================

(eopl:printf "~%== Primera entrega ==~%")

(prueba "literal entero" "42" "42")
(ida-y-vuelta "unparse de un literal" "42")

;; Agregue aquí las pruebas de la primera entrega: literales de cada clase,
;; let, ref/deref/set, begin, if con elif, while, cada primitiva, proc y
;; apply, letrec con recursión mutua. Por ejemplo:
;;
;; (prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
;; (prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
;; (prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")


(ida-y-vuelta "unparse let" "let a = 1, b = 2 in +(a, b) end")
(ida-y-vuelta "unparse if con elif" "if true then 1 elif false then 2 else 3 end")
(ida-y-vuelta "unparse while" "while true do 1 end")
(ida-y-vuelta "unparse proc con tipos" "proc (x: int, r: ref(int)) x end")
(ida-y-vuelta "unparse letrec" "letrec int f (n: int) = n in apply f (1) end")
(ida-y-vuelta "unparse begin" "begin 1; 2; 3 end")
(ida-y-vuelta "unparse set y deref" "let c = ref(0) in begin set c := 5; deref(c) end end")
(ida-y-vuelta "unparse comentarios" "// hola
#| bloque |#
42")
(ida-y-vuelta "unparse cadena" "\"hola\"")
(ida-y-vuelta "unparse atomic" "atomic 1 onfail (m) 2 end")

(prueba "let y suma sin primitivas" "let a = 5 in a end" "5")
(prueba "let paralelo" "let a = 1 in let a = 2, b = a in b end end" "1")
(prueba "begin devuelve el último" "begin 1; 2; 3 end" "3")
(prueba "if verdadero" "if true then 1 else 2 end" "1")
(prueba "if con elif" "if false then 1 elif true then 2 else 3 end" "2")
(prueba "if else" "if false then 1 elif false then 2 else 3 end" "3")
(prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")
(prueba "while falso devuelve unit" "while false do 1 end" "unit")
(prueba-rechazo "while no booleano" "while 1 do 2 end")

;; --- Pruebas del TAD almacén ---
(initialize-store!)
(let ((r0 (newref! (int-val 10)))
      (r1 (newref! (int-val 20))))
  (begin
    (verificar "newref! da índices consecutivos"
               (string-append (number->string r0) (number->string r1)) "01")
    (verificar "deref-ref lee la celda"
               (valor->texto (deref-ref r1)) "20")
    (setref! r0 (int-val 99))
    (verificar "setref! escribe la celda"
               (valor->texto (deref-ref r0)) "99")
    (verificar "setref! no toca otras celdas"
               (valor->texto (deref-ref r1)) "20")
    (verificar "almacen-tamano cuenta las celdas"
               (number->string (almacen-tamano)) "2")))

(verificar "deref-ref de referencia inexistente falla"
           (with-handlers ((exn:fail? (lambda (e) "ERROR")))
             (begin (deref-ref 50) "no falló"))
           "ERROR")
(verificar "setref! de referencia inexistente falla"
           (with-handlers ((exn:fail? (lambda (e) "ERROR")))
             (begin (setref! 50 (int-val 1)) "no falló"))
           "ERROR")
(verificar "newref! rechaza lo que no es valor"
           (with-handlers ((exn:fail? (lambda (e) "ERROR")))
             (begin (newref! 5) "no falló"))
           "ERROR")

;;; ==========================================================
;;; Segunda entrega: transacciones, registros y tipos
;;; ==========================================================

(eopl:printf "~%== Segunda entrega ==~%")

;; Agregue aquí las pruebas de la segunda entrega: bloques atomic que
;; confirman y que revierten, anidamiento, registros, y al menos cinco
;; programas mal tipados con prueba-rechazo o prueba-tipo. Por ejemplo:
;;
;; (prueba-tipo "tipo de un proc" "proc (x: int) x end" "(int -> int)")
;; (prueba-rechazo "suma de int y bool" "+(1, true)")

;;; ==========================================================
;;; Resumen
;;; ==========================================================

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")
