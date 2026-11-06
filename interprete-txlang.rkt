#lang eopl
;Autores: Aura Maria Pelaez Luna,202459422-Valentina Valencia Lopez,202459626-Juan Felipe Aristízabal,2459364

#|
  Interpretador de TxLang-UV.
  Proyecto final del curso Fundamentos de Interpretación y Compilación de
  Lenguajes de Programación, Universidad del Valle.

  El archivo va en nueve secciones, en el orden en que se construye un
  interpretador. La primera entrega llena las secciones 1 a 6 y la 8; la
  segunda agrega al almacén el registro que permite revertir, los registros
  del lenguaje y el chequeador de tipos de la sección 7.

  Lo que hay ahora es el andamiaje mínimo para que las baterías corran: una
  gramática con un solo literal, un value-of de una cláusula y la interfaz
  que usan pruebas-txlang.rkt y ejemplos-anexo.rkt. Todo lo demás lo pone el
  grupo, siguiendo la gramática y la semántica del enunciado.
|#

;;; ==========================================================
;;; 1. Especificación léxica
;;; ==========================================================
;; Los dos tipos de comentario, los literales de cada clase y los
;; identificadores. El comentario de bloque no admite | adentro.

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
    (caracter ("'" any "'") string)))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Un datatype por no terminal: programa, declaración de registro, tipo,
;; expresión y primitiva. Los nombres de las variantes los escoge el grupo.

(define especificacion-gramatical
  '((programa ((arbno decl-registro) expresion) un-programa)

    (decl-registro
     ("record" identificador "{"
      (separated-list identificador ":" tipo ",") "}")
     a-record)

    (tipo ("int") int-tipo)
    (tipo ("float") float-tipo)
    (tipo ("bool") bool-tipo)
    (tipo ("string") string-tipo)
    (tipo ("char") char-tipo)
    (tipo ("unit") unit-tipo)
    (tipo ("ref" "(" tipo ")") ref-tipo)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-tipo)
    (tipo (identificador) registro-tipo)

    (expresion (numero) lit-exp)
    (expresion (flotante) flotante-exp)
    (expresion (cadena) cadena-exp)
    (expresion (caracter) caracter-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) var-exp)
    (expresion ("let" (separated-list identificador "=" expresion ",")
                "in" expresion "end") let-exp)
    (expresion ("ref" "(" expresion ")") ref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("set" expresion ":=" expresion) set-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("if" expresion "then" expresion
                (arbno "elif" expresion "then" expresion)
                "else" expresion "end") if-exp)
    (expresion ("while" expresion "do" expresion "end") while-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (expresion ("proc" "(" (separated-list identificador ":" tipo ",") ")"
                expresion "end") proc-exp)
    (expresion ("apply" expresion "(" (separated-list expresion ",") ")")
               apply-exp)
    (expresion ("letrec"
                (arbno tipo identificador
                       "(" (separated-list identificador ":" tipo ",") ")"
                       "=" expresion)
                "in" expresion "end") letrec-exp)
    (expresion ("atomic" expresion "onfail" "(" identificador ")" expresion "end")
               atomic-exp)
    (expresion ("abort" "(" expresion ")") abort-exp)
    (expresion ("new" identificador "{"
                (separated-list identificador "=" expresion ",") "}") new-exp)
    (expresion ("field" "(" expresion "," identificador ")") field-exp)

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

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; Muestra los define-datatype que SLLGEN generó a partir de la gramática.
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

;;; ==========================================================
;;; 3. Unparse: del árbol de sintaxis abstracta al texto
;;; ==========================================================

;; une una lista de textos poniendo un separador entre cada uno
(define unir
  (lambda (lista separador)
    (cond ((null? lista) "")((null? (cdr lista)) (car lista))
          (else (string-append (car lista) separador(unir (cdr lista) separador))))))

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo)
        (string-append
         (apply string-append
                (map (lambda (d) (string-append (unparse-decl d) "\n")) decls))
         (unparse-expresion cuerpo))))))

(define unparse-decl
  (lambda (decl)
    (cases decl-registro decl
      (a-record (nombre campos tipos)
        (string-append "record " (symbol->string nombre) " { "(unparse-params campos tipos) " }")))))

;; arma "x: int, y: bool"
(define unparse-params
  (lambda (ids tipos)
    (unir (map (lambda (id un-tipo)
                 (string-append (symbol->string id) ": " (unparse-tipo un-tipo)))ids tipos)", ")))

(define unparse-tipo
  (lambda (un-tipo)
    (cases tipo un-tipo
      (int-tipo () "int")
      (float-tipo () "float")
      (bool-tipo () "bool")
      (string-tipo () "string")
      (char-tipo () "char")
      (unit-tipo () "unit")
      (ref-tipo (t) (string-append "ref(" (unparse-tipo t) ")"))
      (proc-tipo (tipos-args tipo-ret)
        (string-append "(" (unir (map unparse-tipo tipos-args) ", ")" -> " (unparse-tipo tipo-ret) ")"))
      (registro-tipo (id) (symbol->string id)))))

(define unparse-prim
  (lambda (prim)
    (cases primitiva prim
      (suma-prim () "+") (resta-prim () "-") (mult-prim () "*")
      (div-prim () "/") (mod-prim () "%")
      (mayor-prim () ">") (menor-prim () "<")
      (mayor-igual-prim () ">=") (menor-igual-prim () "<=")
      (igual-prim () "==") (diferente-prim () "!=")
      (and-prim () "and") (or-prim () "or") (not-prim () "not")
      (concat-prim () "concat") (length-prim () "length")
      (to-string-prim () "to-string") (to-number-prim () "to-number")
      (print-prim () "print"))))

;; arma "a = e1, b = e2" (sirve para let y para new)
(define unparse-ligaduras
  (lambda (ids exps)
    (unir (map (lambda (id e)
                 (string-append (symbol->string id) " = " (unparse-expresion e)))ids exps)", ")))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (lit-exp (n) (number->string n))
      (flotante-exp (f) (number->string f))
      (cadena-exp (c) c)
      (caracter-exp (c) c)
      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")
      (var-exp (id) (symbol->string id))
      (let-exp (ids exps body)
        (string-append "let " (unparse-ligaduras ids exps)" in " (unparse-expresion body) " end"))
      (ref-exp (e) (string-append "ref(" (unparse-expresion e) ")"))
      (deref-exp (e) (string-append "deref(" (unparse-expresion e) ")"))
      (set-exp (e1 e2)
        (string-append "set " (unparse-expresion e1) " := " (unparse-expresion e2)))
      (begin-exp (primera resto)
        (string-append "begin "(unir (map unparse-expresion (cons primera resto)) "; ")" end"))
      (if-exp (prueba entonces pruebas-elif entonces-elif sino)
        (string-append
         "if " (unparse-expresion prueba) " then " (unparse-expresion entonces)
         (apply string-append
                (map (lambda (p r)
                       (string-append " elif " (unparse-expresion p)" then " (unparse-expresion r)))
                     pruebas-elif entonces-elif))" else " (unparse-expresion sino) " end"))
      (while-exp (condicion body)
        (string-append "while " (unparse-expresion condicion)" do " (unparse-expresion body) " end"))
      (prim-exp (prim args)
        (string-append (unparse-prim prim)"(" (unir (map unparse-expresion args) ", ") ")"))
      (proc-exp (ids tipos body)
        (string-append "proc (" (unparse-params ids tipos) ") "(unparse-expresion body) " end"))
      (apply-exp (rator rands)
        (string-append "apply " (unparse-expresion rator)"(" (unir (map unparse-expresion rands) ", ") ")"))
      (letrec-exp (tipos-ret nombres idss tiposs cuerpos body)
        (string-append
         "letrec "
         (unir (map (lambda (tr nombre ids tipos c)
                      (string-append (unparse-tipo tr) " " (symbol->string nombre)
                                     "(" (unparse-params ids tipos) ") = "
                                     (unparse-expresion c)))
                    tipos-ret nombres idss tiposs cuerpos)
               " ")
         " in " (unparse-expresion body) " end"))
      (atomic-exp (body id si-falla)
        (string-append "atomic " (unparse-expresion body)" onfail (" (symbol->string id) ") "
                       (unparse-expresion si-falla) " end"))
      (abort-exp (e) (string-append "abort(" (unparse-expresion e) ")"))
      (new-exp (nombre ids exps)
        (string-append "new " (symbol->string nombre) " { "
                       (unparse-ligaduras ids exps) " }"))
      (field-exp (e id)
        (string-append "field(" (unparse-expresion e) ", " (symbol->string id) ")")))))

;;; ==========================================================
;;; 4. TAD ambiente
;;; ==========================================================
;; Liga identificadores a valores. Su interfaz va aparte de la del almacén.
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (lids (list-of symbol?))
   (lvalue list?)
   (old-env ambiente?))
  ;; marco de un letrec: guarda los ingredientes de las clausuras, no las clausuras
  (ambiente-extendido-recursivo
   (procnames (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (cuerpos (list-of expresion?))
   (old-env ambiente?)))

(define apply-env
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env "No se encuentra ~s" var))
      (ambiente-extendido (lids lvalue old-env)
        (letrec
            ((buscar-variable
              (lambda (lids lvalue)
                (cond
                  ((null? lids) (apply-env old-env var))((equal? (car lids) var) (car lvalue))
                  (else (buscar-variable (cdr lids) (cdr lvalue)))))))
          (buscar-variable lids lvalue)))
      (ambiente-extendido-recursivo (procnames idss cuerpos old-env)
        (letrec
            ((buscar-variable
              (lambda (procnames idss cuerpos)
                (cond
                  ((null? procnames) (apply-env old-env var))
                  ;; la clausura se arma con env (el marco recursivo), no con old-env
                  ((equal? (car procnames) var)
                   (closure (car idss) (car cuerpos) env))
                  (else (buscar-variable (cdr procnames) (cdr idss) (cdr cuerpos)))))))
          (buscar-variable procnames idss cuerpos))))))

;;; ==========================================================
;;; 5. TAD almacén
;;; ==========================================================
;; Celdas que se reservan con ref, se leen con deref y se escriben con set.
;; En la primera entrega las escrituras son definitivas; en la segunda el
;; almacén lleva el registro que permite revertir un bloque atomic. Conviene
;; diseñar la interfaz desde ahora pensando en eso.

;;; ==========================================================
;;; 6. Valores expresados
;;; ==========================================================
;; Un define-datatype con los valores del lenguaje y valor->texto, que es
;; lo que imprime correr.

(define valor->texto
  (lambda (v)
    (cond ((number? v) (number->string v))
          (else (eopl:error 'valor->texto "no sé escribir el valor ~s" v)))))

;;; ==========================================================
;;; 7. Chequeador de tipos (segunda entrega)
;;; ==========================================================
;; Recibe el programa y lo devuelve si está bien tipado; si no, aborta con
;; eopl:error antes de que se evalúe. Mientras no exista, deja pasar todo.

(define chequear-programa
  (lambda (pgm) pgm))

;; Tipo del programa, escrito como texto: "int", "ref(bool)", "(int -> int)".
(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "el chequeador de tipos llega en la segunda entrega")))

;;; ==========================================================
;;; 8. Interpretador
;;; ==========================================================

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) n))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (cases programa pgm
          (un-programa (cuerpo) (value-of cuerpo 'ambiente-vacio)))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))