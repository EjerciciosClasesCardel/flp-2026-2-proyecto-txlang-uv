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

;; ---- Ambientes, valores, let, clausuras y letrec ----------------------

;; Prueba directa: no pasa por el parser. Si el procedimiento aborta, el
;; resultado es el mensaje precedido de ERROR.
(define prueba-directa
  (lambda (etiqueta thunk esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (thunk))
               esperado)))

(define termina-con?
  (lambda (texto sufijo)
    (let ((n (string-length texto)) (m (string-length sufijo)))
      (and (>= n m) (string=? (substring texto (- n m) n) sufijo)))))

;; El procedimiento debe abortar con un mensaje que termina en el sufijo.
(define prueba-directa-error
  (lambda (etiqueta thunk sufijo)
    (let ((r (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
               (thunk))))
      (if (and (empieza-con-error? r) (termina-con? r sufijo))
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un error que termine en ~s y llegó ~s~%"
                              etiqueta sufijo r))))))

;; Sin gramática: sirven desde ya, con los ASTs armados a mano.
(define amb-prueba
  (ambiente-extendido '(x y) (list (valor-entero 1) (valor-booleano #t))
    (ambiente-extendido '(x z) (list (valor-cadena "viejo") (valor-flotante 2.5))
      (ambiente-vacio))))

(define amb-rec
  (ambiente-recursivo '(f) (list '(a)) (list (lit-exp 7)) (ambiente-vacio)))

(prueba-directa "apply-env: la ligadura nueva tapa a la vieja"
  (lambda () (valor->texto (apply-env amb-prueba 'x))) "1")
(prueba-directa "apply-env: busca en el ambiente exterior"
  (lambda () (valor->texto (apply-env amb-prueba 'z))) "2.5")
(prueba-directa-error "apply-env: variable inexistente"
  (lambda () (apply-env amb-prueba 'w)) "No se encuentra la variable w")
(prueba-directa "ambiente recursivo: la clausura se ve a sí misma"
  (lambda ()
    (cases valor (apply-env amb-rec 'f)
      (valor-clausura (params cuerpo amb) (valor->texto (apply-env amb 'f)))
      (else "no es clausura")))
  "<procedimiento>")
(prueba-directa "aplicar-clausura de un procedimiento recursivo"
  (lambda () (valor->texto (aplicar-clausura (apply-env amb-rec 'f) (list (valor-entero 0)))))
  "7")
(prueba-directa "valor->texto de cada valor"
  (lambda ()
    (apply string-append
           (map (lambda (v) (string-append (valor->texto v) "|"))
                (list (valor-entero 42) (valor-flotante 2.5) (valor-cadena "hola")
                      (valor-caracter #\a) (valor-booleano #f) (valor-unit)))))
  "42|2.5|hola|a|false|unit|")
(prueba-directa-error "aplicar-clausura con argumentos de menos"
  (lambda ()
    (aplicar-clausura (eval-proc '(a b) (lit-exp 1) (ambiente-vacio)) (list (valor-entero 1))))
  "llegaron 1")
(prueba-directa-error "eval-apply con un operador que no es procedimiento"
  (lambda () (eval-apply (lit-exp 5) '() (ambiente-vacio)))
  "no es un procedimiento")

;; Con gramática: pasan cuando value-of llame a eval-let, eval-proc,
;; eval-apply y eval-letrec (y la gramática tenga var-exp, let, proc, apply
;; y letrec). Si la sintaxis de letrec con varios procedimientos es otra,
;; se ajusta el texto de la prueba 4.
(prueba "1. let y ambiente"
  "let a = 1, b = 2 in +(a, b) end" "3")
(prueba "2. las ligaduras de un let no se ven entre sí"
  "let x = 1 in let x = 10, y = x in +(x, y) end end" "11")
(prueba "3. la clausura usa el ambiente donde se creó"
  "let x = 5 in let f = proc (y: int) +(x, y) end in let x = 100 in apply f (1) end end end" "6")
(prueba "4. letrec con recursión mutua"
  "letrec bool par (n: int) = if ==(n, 0) then true else apply impar (-(n, 1)) end,
          bool impar (n: int) = if ==(n, 0) then false else apply par (-(n, 1)) end
   in apply par (10) end" "true")
(prueba "5. letrec con recursión simple"
  "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact (-(n, 1))) end
   in apply fact (5) end" "120")
(prueba-rechazo "variable que no existe" "let a = 1 in b end")
(prueba-rechazo "apply de algo que no es procedimiento" "apply 5 (1)")
(prueba-rechazo "apply con argumentos de más"
  "let f = proc (x: int) x end in apply f (1, 2) end")

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