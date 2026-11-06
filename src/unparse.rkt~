#lang eopl
;Autores: Oliver De Jesus Arboleda Baez 2459684, Maria Fernanda Betancourt Montoya 2459510, Kevin Andres Rosero Romo 2459554

;; ==========================================================
;; 1. Especificación léxica

;; Comentarios:
;; - De línea: // hasta el fin del renglón.
;; - De bloque: #| ... |#.

(define especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario-linea ("//" (arbno (not #\newline))) skip)
    (comentario-bloque ("#|" (arbno (not #\|)) "|#") skip)
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)
    (caracter ("'" (not #\') "'") string)))

;; ==========================================================
;; 2. Especificación gramatical

(define especificacion-gramatical
  '((programa ((arbno decl-registro) expresion) un-programa)

    (decl-registro
     ("record" identificador "{"
      (separated-list identificador ":" tipo ",") "}")
     decl-reg)

    ;; Tipos
    (tipo ("int") int-tipo)
    (tipo ("float") float-tipo)
    (tipo ("bool") bool-tipo)
    (tipo ("string") string-tipo)
    (tipo ("char") char-tipo)
    (tipo ("unit") unit-tipo)
    (tipo ("ref" "(" tipo ")") ref-tipo)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-tipo)
    (tipo (identificador) registro-tipo)

    ;; Literales y variables
    (expresion (numero) lit-exp)
    (expresion (flotante) flot-exp)
    (expresion (cadena) cadena-exp)
    (expresion (caracter) car-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) var-exp)

    ;; Ligaduras
    (expresion
     ("let" (separated-list identificador "=" expresion ",")
      "in" expresion "end")
     let-exp)

    ;; Referencias y asignación
    (expresion ("ref" "(" expresion ")") ref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("set" expresion ":=" expresion) set-exp)

    ;; Secuenciación
    (expresion
     ("begin" expresion (arbno ";" expresion) "end")
     begin-exp)

    ;; Condicional
    (expresion
     ("if" expresion "then" expresion
      (arbno "elif" expresion "then" expresion)
      "else" expresion "end")
     if-exp)

    ;; Ciclo
    (expresion ("while" expresion "do" expresion "end") while-exp)

    ;; Primitivas
    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)

    ;; Procedimientos
    (expresion
     ("proc" "(" (separated-list identificador ":" tipo ",") ")"
      expresion "end")
     proc-exp)

    ;; Aplicación
    (expresion
     ("apply" expresion "(" (separated-list expresion ",") ")")
     apply-exp)

    ;; Recursión
    (expresion
     ("letrec"
      (arbno tipo identificador
             "(" (separated-list identificador ":" tipo ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)

    ;; Transacciones (segunda entrega)
    (expresion
     ("atomic" expresion "onfail" "(" identificador ")" expresion "end")
     atomic-exp)

    (expresion ("abort" "(" expresion ")") abort-exp)

    ;; Registros (segunda entrega)
    (expresion
     ("new" identificador "{"
      (separated-list identificador "=" expresion ",") "}")
     new-exp)

    (expresion
     ("field" "(" expresion "," identificador ")")
     field-exp)

    ;; Operadores primitivos
    (primitiva ("+") suma-prim)
    (primitiva ("-") resta-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">=") mayor-igual-prim)
    (primitiva ("<=") menor-igual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)
    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)
    (primitiva ("to-string") to-string-prim)
    (primitiva ("to-number") to-number-prim)
    (primitiva ("print") print-prim)))

;; ==========================================================
;; 3. Generación de datatypes y parser

(sllgen:make-define-datatypes
 especificacion-lexica
 especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser
   especificacion-lexica
   especificacion-gramatical))

;; Permite consultar los datatypes generados por SLLGEN.
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes
     especificacion-lexica
     especificacion-gramatical)))

;; ==========================================================
;; 4. Funciones auxiliares para cadenas y caracteres

(define texto-de-cadena
  (lambda (s)
    (let ((n (string-length s)))
      (if (and (>= n 2)
               (char=? (string-ref s 0) #\")
               (char=? (string-ref s (- n 1)) #\"))
          (substring s 1 (- n 1))
          s))))

(define caracter-de-token
  (lambda (s)
    (if (and (= (string-length s) 3)
             (char=? (string-ref s 0) #\'))
        (string-ref s 1)
        (string-ref s 0))))

;; ==========================================================
;; 5. Exportación del módulo

(provide (all-defined-out))
