#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562
 
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
 
;; --- Literales ---
(prueba "literal entero" "42" "42")
(prueba "literal negativo" "-7" "-7")
(prueba "literal flotante" "3.14" "3.14")
(prueba "literal cadena" "\"hola\"" "hola")
(prueba "literal caracter" "'a'" "a")
(prueba "literal true" "true" "true")
(prueba "literal false" "false" "false")
(prueba "literal unit" "unit" "unit")
(prueba "comentarios de linea y bloque" "// hola
#| bloque |#
42" "42")
 
;; --- unparse ---
(ida-y-vuelta "unparse de un literal" "42")
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
 
;; --- let, begin, if, while ---
(prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
(prueba "let y suma sin primitivas" "let a = 5 in a end" "5")
(prueba "let paralelo" "let a = 1 in let a = 2, b = a in b end end" "1")
(prueba-rechazo "variable no ligada" "x")
(prueba "begin devuelve el último" "begin 1; 2; 3 end" "3")
(prueba "if verdadero" "if true then 1 else 2 end" "1")
(prueba "if con elif" "if false then 1 elif true then 2 else 3 end" "2")
(prueba "if else" "if false then 1 elif false then 2 else 3 end" "3")
(prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")
(prueba "while falso devuelve unit" "while false do 1 end" "unit")
(prueba-rechazo "while no booleano" "while 1 do 2 end")
 
;; --- Primitivas numéricas ---
(prueba "suma" "+(2, 3)" "5")
(prueba "resta y mult" "*(-(10, 4), 7)" "42")
(prueba "division entera" "/(7, 2)" "3")
(prueba "division entera negativa" "/(-7, 2)" "-3")
(prueba "modulo" "%(7, 3)" "1")
(prueba "suma de flotantes" "+(1.5, 2.5)" "4.0")
(prueba "mult de flotantes" "*(2.5, 2.0)" "5.0")
(prueba "division flotante" "/(7.0, 2.0)" "3.5")
(prueba-rechazo "division por cero" "/(1, 0)")
(prueba-rechazo "division flotante por cero" "/(1.0, 0.0)")
(prueba-rechazo "modulo por cero" "%(5, 0)")
(prueba-rechazo "modulo de flotantes" "%(5.0, 2.0)")
(prueba-rechazo "suma int y bool" "+(1, true)")
(prueba-rechazo "suma int y float" "+(1, 2.0)")
(prueba-rechazo "suma con aridad incorrecta" "+(1)")
 
;; --- Primitivas relacionales y booleanas ---
(prueba "mayor" ">(3, 2)" "true")
(prueba "menor-igual falso" "<=(3, 2)" "false")
(prueba "mayor-igual" ">=(2, 2)" "true")
(prueba "menor" "<(1, 2)" "true")
(prueba "igualdad" "==(2, 2)" "true")
(prueba "diferente" "!=(2, 2)" "false")
(prueba "igualdad de cadenas" "==(\"a\", \"a\")" "true")
(prueba "igualdad entre tipos distintos" "==(1, 1.0)" "false")
(prueba-rechazo "relacional sobre booleanos" "<(true, false)")
(prueba "and" "and(true, false)" "false")
(prueba "or" "or(true, false)" "true")
(prueba "not" "not(false)" "true")
(prueba-rechazo "and valida los dos operandos" "and(true, 1)")
(prueba-rechazo "not sobre un entero" "not(1)")
 
;; --- Cadenas, conversión y print ---
(prueba "concat" "concat(\"ho\", \"la\")" "hola")
(prueba "length" "length(\"hola\")" "4")
(prueba-rechazo "length sobre un entero" "length(5)")
(prueba-rechazo "concat con un entero" "concat(\"a\", 1)")
(prueba "to-string de entero" "to-string(42)" "42")
(prueba "to-string de booleano" "to-string(true)" "true")
(prueba "to-number entero" "to-number(\"42\")" "42")
(prueba "to-number flotante" "to-number(\"3.5\")" "3.5")
(prueba-rechazo "to-number invalido" "to-number(\"abc\")")
(prueba "print devuelve unit" "print(\"hola\")" "unit")
 
;; --- ref, deref y set ---
(prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
(prueba "dos escrituras"
  "let c = ref(0) in begin set c := +(deref(c), 1); set c := +(deref(c), 1); deref(c) end end"
  "2")
(prueba "alias de celda"
  "let a = ref(1) in let b = a in begin set b := 9; deref(a) end end end"
  "9")
(prueba "set devuelve unit" "let c = ref(0) in set c := 1 end" "unit")
(prueba-rechazo "deref de no referencia" "deref(5)")
(prueba-rechazo "set sobre no referencia" "set 5 := 1")
(prueba "while con contador"
  "let i = ref(0), s = ref(0) in begin while <(deref(i), 5) do begin set s := +(deref(s), deref(i)); set i := +(deref(i), 1) end end; deref(s) end end"
  "10")
 
;; --- Procedimientos y clausuras ---
(prueba "apply de proc anonimo" "apply proc (x: int, y: int) -(x, y) end (10, 3)" "7")
(prueba "proc ligado con let" "let f = proc (x: int) +(x, 1) end in apply f (41) end" "42")
(prueba "alcance lexico"
  "let x = 5 in let f = proc (y: int) +(x, y) end in let x = 100 in apply f (1) end end end"
  "6")
(prueba "referencia como argumento"
  "let celda = ref(10), duplicar = proc (r: ref(int)) set r := *(deref(r), 2) end in begin apply duplicar (celda); apply duplicar (celda); deref(celda) end end"
  "40")
(prueba-rechazo "aridad incorrecta" "apply proc (x: int) x end (1, 2)")
(prueba-rechazo "apply de no procedimiento" "apply 5 (1)")
 
;; --- letrec ---
(prueba "factorial"
  "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact (-(n, 1))) end in apply fact (5) end"
  "120")
(prueba "fibonacci"
  "letrec int fib (n: int) = if <(n, 2) then n else +(apply fib (-(n, 1)), apply fib (-(n, 2))) end in apply fib (10) end"
  "55")
(prueba "even/odd mutuas (even)"
  "letrec bool even (n: int) = if ==(n, 0) then true else apply odd (-(n, 1)) end bool odd (n: int) = if ==(n, 0) then false else apply even (-(n, 1)) end in apply even (10) end"
  "true")
(prueba "even/odd mutuas (odd)"
  "letrec bool even (n: int) = if ==(n, 0) then true else apply odd (-(n, 1)) end bool odd (n: int) = if ==(n, 0) then false else apply even (-(n, 1)) end in apply odd (7) end"
  "true")
 
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
