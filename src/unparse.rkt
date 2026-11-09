#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562

(require "sintaxis.rkt")
(provide (all-defined-out))

;; unir : listaDeStrings x string -> string
(define unir
  (lambda (lista sep)
    (cond ((null? lista) "")
          ((null? (cdr lista)) (car lista))
          (else (string-append (car lista) sep (unir (cdr lista) sep))))))

;; map2 : (a b -> c) x listaA x listaB -> listaC
(define map2
  (lambda (f l1 l2)
    (if (null? l1)
        '()
        (cons (f (car l1) (car l2)) (map2 f (cdr l1) (cdr l2))))))

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo)
        (let ((txt (unparse-expresion cuerpo)))
          (if (null? decls)
              txt
              (string-append (unir (map unparse-decl decls) "\n") "\n" txt)))))))

(define unparse-decl
  (lambda (d)
    (cases decl-registro d
      (registro-decl (nombre ids tipos)
        (string-append "record " (symbol->string nombre) " { "
                       (unparse-params ids tipos) " }")))))

;; "x: int, y: bool"
(define unparse-params
  (lambda (ids tipos)
    (unir (map2 (lambda (i t)
                  (string-append (symbol->string i) ": " (unparse-tipo t)))
                ids tipos)
          ", ")))

(define unparse-tipo
  (lambda (t)
    (cases tipo t
      (int-tipo () "int")
      (float-tipo () "float")
      (bool-tipo () "bool")
      (string-tipo () "string")
      (char-tipo () "char")
      (unit-tipo () "unit")
      (ref-tipo (t1) (string-append "ref(" (unparse-tipo t1) ")"))
      (proc-tipo (args ret)
        (string-append "(" (unir (map unparse-tipo args) ", ")
                       " -> " (unparse-tipo ret) ")"))
      (registro-tipo (nombre) (symbol->string nombre)))))

(define unparse-lista
  (lambda (exps)
    (unir (map unparse-expresion exps) ", ")))

(define unparse-elifs
  (lambda (conds exps)
    (if (null? conds)
        ""
        (string-append " elif " (unparse-expresion (car conds))
                       " then " (unparse-expresion (car exps))
                       (unparse-elifs (cdr conds) (cdr exps))))))

(define unparse-funs
  (lambda (rets nombres pids ptipos cuerpos)
    (if (null? rets)
        '()
        (cons (string-append (unparse-tipo (car rets)) " "
                             (symbol->string (car nombres))
                             " (" (unparse-params (car pids) (car ptipos)) ") = "
                             (unparse-expresion (car cuerpos)))
              (unparse-funs (cdr rets) (cdr nombres) (cdr pids)
                            (cdr ptipos) (cdr cuerpos))))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (lit-exp (n) (number->string n))
      (flotante-exp (n) (number->string n))
      (cadena-exp (s) s)
      (caracter-exp (c) c)
      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")
      (var-exp (id) (symbol->string id))
      (let-exp (ids exps cuerpo)
        (string-append
         "let "
         (unir (map2 (lambda (i e)
                       (string-append (symbol->string i) " = " (unparse-expresion e)))
                     ids exps)
               ", ")
         " in " (unparse-expresion cuerpo) " end"))
      (ref-exp (e) (string-append "ref(" (unparse-expresion e) ")"))
      (deref-exp (e) (string-append "deref(" (unparse-expresion e) ")"))
      (set-exp (r v)
        (string-append "set " (unparse-expresion r) " := " (unparse-expresion v)))
      (begin-exp (primera resto)
        (string-append "begin "
                       (unir (cons (unparse-expresion primera)
                                   (map unparse-expresion resto))
                             "; ")
                       " end"))
      (if-exp (c t conds exps e)
        (string-append "if " (unparse-expresion c)
                       " then " (unparse-expresion t)
                       (unparse-elifs conds exps)
                       " else " (unparse-expresion e) " end"))
      (while-exp (c cuerpo)
        (string-append "while " (unparse-expresion c)
                       " do " (unparse-expresion cuerpo) " end"))
      (prim-exp (prim rands)
        (string-append (unparse-prim prim) "(" (unparse-lista rands) ")"))
      (proc-exp (ids tipos cuerpo)
        (string-append "proc (" (unparse-params ids tipos) ") "
                       (unparse-expresion cuerpo) " end"))
      (apply-exp (rator rands)
        (string-append "apply " (unparse-expresion rator)
                       " (" (unparse-lista rands) ")"))
      (letrec-exp (rets nombres pids ptipos cuerpos cuerpo)
        (string-append "letrec "
                       (unir (unparse-funs rets nombres pids ptipos cuerpos) " ")
                       " in " (unparse-expresion cuerpo) " end"))
      (atomic-exp (cuerpo id manejador)
        (string-append "atomic " (unparse-expresion cuerpo)
                       " onfail (" (symbol->string id) ") "
                       (unparse-expresion manejador) " end"))
      (abort-exp (e) (string-append "abort(" (unparse-expresion e) ")"))
      (new-exp (nombre ids exps)
        (string-append
         "new " (symbol->string nombre) " { "
         (unir (map2 (lambda (i e)
                       (string-append (symbol->string i) " = " (unparse-expresion e)))
                     ids exps)
               ", ")
         " }"))
      (field-exp (e id)
        (string-append "field(" (unparse-expresion e) ", "
                       (symbol->string id) ")")))))

(define unparse-prim
  (lambda (p)
    (cases primitiva p
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