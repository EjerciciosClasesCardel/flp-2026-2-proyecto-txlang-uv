#lang eopl
;Autores:  Juan Eduardo Calderon Jaramillo 2611001

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
    (numero (digit (arbno digit)) number)
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Un datatype por no terminal: programa, declaración de registro, tipo,
;; expresión y primitiva. Los nombres de las variantes los escoge el grupo.

(define especificacion-gramatical
  '((programa (expresion) un-programa)
    (expresion (numero) lit-exp)))

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

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo) (unparse-expresion cuerpo)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (lit-exp (n) (number->string n)))))

;;; ==========================================================
;;; 4. TAD ambiente
;;; ==========================================================
;; Liga identificadores a valores. Su interfaz va aparte de la del almacén.
;;
;; Interfaz:
;;   (ambiente-vacio)                          -> ambiente sin ligaduras
;;   (ambiente-extendido ids vals amb)         -> amb más las ligaduras ids[i] = vals[i]
;;   (ambiente-recursivo nombres params cuerpos amb)
;;                                             -> amb más procedimientos que se ven a sí
;;                                                mismos (letrec, recursión simple y mutua)
;;   (apply-env amb id)                        -> valor ligado a id
;;
;; ids/nombres son listas de símbolos; vals, una lista de valores (sección 6);
;; params, una lista de listas de símbolos (una por procedimiento) y cuerpos,
;; una lista de expresiones.

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids list?)
   (vals list?)
   (amb ambiente?))
  (ambiente-recursivo
   (nombres list?)
   (lista-params list?)
   (cuerpos list?)
   (amb ambiente?)))

;; Posición (desde 0) de id en la lista de símbolos, o #f si no está.
(define posicion-de
  (lambda (id ids)
    (letrec ((buscar (lambda (lst i)
                       (cond ((null? lst) #f)
                             ((eqv? (car lst) id) i)
                             (else (buscar (cdr lst) (+ i 1)))))))
      (buscar ids 0))))

;; Elemento en la posición i de la lista.
(define elemento-en
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (elemento-en (cdr lst) (- i 1)))))

;; En un ambiente recursivo no se guarda la clausura, sino sus partes: cada
;; vez que se busca el nombre se arma una clausura cuyo ambiente de creación
;; es el ambiente recursivo completo. Así el procedimiento se ve a sí mismo y
;; a los demás del mismo letrec.
(define apply-env
  (lambda (amb id)
    (cases ambiente amb
      (ambiente-vacio ()
        (eopl:error 'apply-env "No se encuentra la variable ~s" id))
      (ambiente-extendido (ids vals amb-ext)
        (let ((pos (posicion-de id ids)))
          (if pos
              (elemento-en vals pos)
              (apply-env amb-ext id))))
      (ambiente-recursivo (nombres lista-params cuerpos amb-ext)
        (let ((pos (posicion-de id nombres)))
          (if pos
              (valor-clausura (elemento-en lista-params pos)
                              (elemento-en cuerpos pos)
                              amb)
              (apply-env amb-ext id)))))))

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
;;
;; valor-referencia guarda la referencia tal como la entregue el almacén
;; (sección 5); aquí no se inspecciona, solo se imprime.
;; valor-clausura guarda los parámetros (lista de símbolos), el cuerpo (una
;; expresión) y el ambiente en que se creó el procedimiento.

(define cualquier-cosa? (lambda (x) #t))

(define-datatype valor valor?
  (valor-entero (n integer?))
  (valor-flotante (x real?))
  (valor-cadena (s string?))
  (valor-caracter (c char?))
  (valor-booleano (b boolean?))
  (valor-unit)
  (valor-referencia (ref cualquier-cosa?))
  (valor-clausura
   (params list?)
   (cuerpo cualquier-cosa?)
   (amb ambiente?)))

;; Texto de cualquier valor. Las cadenas y los caracteres salen tal cual, sin
;; comillas; unit sale como "unit".
(define valor->texto
  (lambda (v)
    (cases valor v
      (valor-entero (n) (number->string n))
      (valor-flotante (x) (number->string x))
      (valor-cadena (s) s)
      (valor-caracter (c) (list->string (list c)))
      (valor-booleano (b) (if b "true" "false"))
      (valor-unit () "unit")
      (valor-referencia (r)
        (if (number? r)
            (string-append "<ref " (number->string r) ">")
            "<ref>"))
      (valor-clausura (params cuerpo amb) "<procedimiento>"))))

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
      (lit-exp (n) (valor-entero n)))))

;; Funciones auxiliares de value-of para let, proc, apply y letrec. Reciben
;; las piezas del árbol ya separadas, así que no dependen de cómo se llamen
;; las variantes de la gramática. Para conectarlas, cada cláusula de value-of
;; las llama con los campos de su variante, por ejemplo:
;;
;;   (var-exp (id) (apply-env amb id))
;;   (let-exp (ids exps cuerpo) (eval-let ids exps cuerpo amb))
;;   (proc-exp (params cuerpo) (eval-proc params cuerpo amb))
;;   (apply-exp (operador operandos) (eval-apply operador operandos amb))
;;   (letrec-exp (nombres lista-params cuerpos cuerpo)
;;     (eval-letrec nombres lista-params cuerpos cuerpo amb))
;;
;; params y lista-params son listas de símbolos: si la gramática lleva tipos
;; en los parámetros, se separan los identificadores antes de llamar.

;; Evalúa las expresiones de izquierda a derecha, todas en amb, y devuelve la
;; lista de valores. El orden importa porque una expresión puede escribir en
;; el almacén o imprimir.
(define evaluar-lista
  (lambda (exps amb)
    (if (null? exps)
        '()
        (let ((v (value-of (car exps) amb)))
          (cons v (evaluar-lista (cdr exps) amb))))))

;; let: todas las ligaduras se evalúan en el ambiente exterior, así que una
;; no ve a las otras. Solo el cuerpo ve las variables nuevas.
(define eval-let
  (lambda (ids exps cuerpo amb)
    (let ((vals (evaluar-lista exps amb)))
      (value-of cuerpo (ambiente-extendido ids vals amb)))))

;; proc: la clausura guarda el ambiente en que se crea (alcance léxico).
(define eval-proc
  (lambda (params cuerpo amb)
    (valor-clausura params cuerpo amb)))

(define es-clausura?
  (lambda (v)
    (cases valor v
      (valor-clausura (params cuerpo amb) #t)
      (else #f))))

;; Aplica una clausura a valores ya evaluados: el cuerpo corre en el ambiente
;; de creación extendido con los parámetros, no en el de quien llama.
(define aplicar-clausura
  (lambda (clausura args)
    (cases valor clausura
      (valor-clausura (params cuerpo amb-creacion)
        (if (= (length params) (length args))
            (value-of cuerpo (ambiente-extendido params args amb-creacion))
            (eopl:error 'apply
                        "La cantidad de argumentos no coincide: se esperaban ~s y llegaron ~s"
                        (length params) (length args))))
      (else
       (eopl:error 'apply "No se puede aplicar ~a: no es un procedimiento"
                   (valor->texto clausura))))))

;; apply: evalúa el operador y exige que sea una clausura antes de tocar los
;; operandos; luego evalúa los operandos de izquierda a derecha y aplica.
(define eval-apply
  (lambda (operador operandos amb)
    (let ((proc (value-of operador amb)))
      (if (es-clausura? proc)
          (aplicar-clausura proc (evaluar-lista operandos amb))
          (eopl:error 'apply "No se puede aplicar ~a: no es un procedimiento"
                      (valor->texto proc))))))

;; letrec: el cuerpo corre en un ambiente recursivo, donde los nombres ligan
;; procedimientos que se ven a sí mismos y entre sí (recursión mutua).
(define eval-letrec
  (lambda (nombres lista-params cuerpos cuerpo-letrec amb)
    (value-of cuerpo-letrec
              (ambiente-recursivo nombres lista-params cuerpos amb))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (cases programa pgm
          (un-programa (cuerpo) (value-of cuerpo (ambiente-vacio))))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))