
#lang eopl
;Autores: Oliver De Jesus Arboleda Baez 2459684, Maria Fernanda Betancourt Montoya 2459510, Kevin Andres Rosero Romo 2459554

(require "sintaxis.rkt")

;; ==========================================================
;; 1. Función auxiliar para unir listas de textos
;; ==========================================================

(define unir-con
  (lambda (lista separador)
    (cond
      ((null? lista) "")
      ((null? (cdr lista)) (car lista))
      (else
       (string-append
        (car lista)
        separador
        (unir-con (cdr lista) separador))))))

;; ==========================================================
;; 2. Unparse del programa completo
;; ==========================================================

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo)
        (unir-con
         (append
          (map unparse-decl-registro decls)
          (list (unparse-expresion cuerpo)))
         "\n")))))

;; ==========================================================
;; 3. Unparse de declaraciones de registros
;; ==========================================================

(define unparse-decl-registro
  (lambda (decl)
    (cases decl-registro decl
      (decl-reg (nombre campos tipos)
        (string-append
         "record "
         (symbol->string nombre)
         " { "
         (unparse-parametros campos tipos)
         " }")))))

;; ==========================================================
;; 4. Unparse de tipos
;; ==========================================================

(define unparse-tipo
  (lambda (t)
    (cases tipo t
      (int-tipo () "int")
      (float-tipo () "float")
      (bool-tipo () "bool")
      (string-tipo () "string")
      (char-tipo () "char")
      (unit-tipo () "unit")
      (ref-tipo (t1)
        (string-append "ref(" (unparse-tipo t1) ")"))
      (proc-tipo (args ret)
        (string-append
         "("
         (unir-con (map unparse-tipo args) ", ")
         " -> "
         (unparse-tipo ret)
         ")"))
      (registro-tipo (nombre)
        (symbol->string nombre)))))

;; ==========================================================
;; 5. Unparse de parámetros
;; ==========================================================

;; Convierte, por ejemplo:
;; (x int, y bool) en "x: int, y: bool"

(define unparse-parametros
  (lambda (ids tipos)
    (unir-con
     (map
      (lambda (id t)
        (string-append
         (symbol->string id)
         ": "
         (unparse-tipo t)))
      ids
      tipos)
     ", ")))

;; ==========================================================
;; 6. Unparse de ligaduras
;; ==========================================================

;; Convierte, por ejemplo:
;; (a e1, b e2) en "a = e1, b = e2"

(define unparse-ligaduras
  (lambda (ids exps)
    (unir-con
     (map
      (lambda (id e)
        (string-append
         (symbol->string id)
         " = "
         (unparse-expresion e)))
      ids
      exps)
     ", ")))

;; ==========================================================
;; 7. Unparse de primitivas
;; ==========================================================

(define unparse-primitiva
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () "+")
      (resta-prim () "-")
      (mult-prim () "*")
      (div-prim () "/")
      (mod-prim () "%")
      (mayor-prim () ">")
      (menor-prim () "<")
      (mayor-igual-prim () ">=")
      (menor-igual-prim () "<=")
      (igual-prim () "==")
      (diferente-prim () "!=")
      (and-prim () "and")
      (or-prim () "or")
      (not-prim () "not")
      (concat-prim () "concat")
      (length-prim () "length")
      (to-string-prim () "to-string")
      (to-number-prim () "to-number")
      (print-prim () "print"))))

;; ==========================================================
;; 8. Unparse de expresiones
;; ==========================================================

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp

      ;; Literales
      (lit-exp (n)
        (number->string n))

      (flot-exp (n)
        (number->string n))

      (cadena-exp (c)
        (string-append "\"" (texto-de-cadena c) "\""))

      (car-exp (c)
        (string-append "'" (string (caracter-de-token c)) "'"))

      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")

      ;; Variables
      (var-exp (id)
        (symbol->string id))

      ;; Let
      (let-exp (ids exps cuerpo)
        (string-append
         "let "
         (unparse-ligaduras ids exps)
         " in "
         (unparse-expresion cuerpo)
         " end"))

      ;; Referencias
      (ref-exp (e)
        (string-append
         "ref("
         (unparse-expresion e)
         ")"))

      (deref-exp (e)
        (string-append
         "deref("
         (unparse-expresion e)
         ")"))

      ;; Asignación
      (set-exp (e1 e2)
        (string-append
         "set "
         (unparse-expresion e1)
         " := "
         (unparse-expresion e2)))

      ;; Secuencia
      (begin-exp (primera resto)
        (string-append
         "begin "
         (unir-con
          (map unparse-expresion (cons primera resto))
          "; ")
         " end"))

      ;; Condicional
      (if-exp (prueba rama-then elif-pruebas elif-ramas rama-else)
        (string-append
         "if "
         (unparse-expresion prueba)
         " then "
         (unparse-expresion rama-then)

         (apply string-append
                (map
                 (lambda (p r)
                   (string-append
                    " elif "
                    (unparse-expresion p)
                    " then "
                    (unparse-expresion r)))
                 elif-pruebas
                 elif-ramas))

         " else "
         (unparse-expresion rama-else)
         " end"))

      ;; Ciclo while
      (while-exp (prueba cuerpo)
        (string-append
         "while "
         (unparse-expresion prueba)
         " do "
         (unparse-expresion cuerpo)
         " end"))

      ;; Primitivas
      (prim-exp (prim args)
        (string-append
         (unparse-primitiva prim)
         "("
         (unir-con (map unparse-expresion args) ", ")
         ")"))

      ;; Procedimientos
      (proc-exp (ids tipos cuerpo)
        (string-append
         "proc ("
         (unparse-parametros ids tipos)
         ") "
         (unparse-expresion cuerpo)
         " end"))

      ;; Aplicación
      (apply-exp (rator rands)
        (string-append
         "apply "
         (unparse-expresion rator)
         " ("
         (unir-con (map unparse-expresion rands) ", ")
         ")"))

      ;; Recursión
      (letrec-exp (tipos-ret nombres ids-lista tipos-lista cuerpos cuerpo)
        (string-append
         "letrec "
         (unir-con
          (map
           (lambda (t nombre ids tipos c)
             (string-append
              (unparse-tipo t)
              " "
              (symbol->string nombre)
              " ("
              (unparse-parametros ids tipos)
              ") = "
              (unparse-expresion c)))
           tipos-ret
           nombres
           ids-lista
           tipos-lista
           cuerpos)
          " ")
         " in "
         (unparse-expresion cuerpo)
         " end"))

      ;; Transacciones (segunda entrega)
      (atomic-exp (cuerpo m manejador)
        (string-append
         "atomic "
         (unparse-expresion cuerpo)
         " onfail ("
         (symbol->string m)
         ") "
         (unparse-expresion manejador)
         " end"))

      (abort-exp (e)
        (string-append
         "abort("
         (unparse-expresion e)
         ")"))

      ;; Registros (segunda entrega)
      (new-exp (nombre campos exps)
        (string-append
         "new "
         (symbol->string nombre)
         " { "
         (unparse-ligaduras campos exps)
         " }"))

      (field-exp (e campo)
        (string-append
         "field("
         (unparse-expresion e)
         ", "
         (symbol->string campo)
         ")")))))

;; ==========================================================
;; 9. Exportación del módulo
;; ==========================================================

(provide (all-defined-out))
