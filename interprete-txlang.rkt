#lang eopl
;Autores:  Juan Eduardo Calderon Jaramillo 2611001, Carlos Humberto Gutierrez 2059817

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
;; ;; Interfaz:
;;   (inicializar-almacen!)         -> deja el almacén vacío
;;   (almacen-reservar! v)          -> crea una celda con v y devuelve su referencia
;;   (almacen-leer ref)             -> contenido de la celda ref
;;   (almacen-escribir! ref v)      -> reemplaza el contenido de la celda ref
;;
;; Representación: una lista de valores guardada en una variable global.
;; La referencia n es la posición n de la lista. Todo el que toque la
;; memoria pasa por estas cuatro funciones, así que la segunda entrega puede
;; agregar el registro de cambios aquí adentro sin tocar el resto.

(define the-almacen 'sin-inicializar)

(define inicializar-almacen!
  (lambda ()
    (set! the-almacen '())))

(define referencia-valida?
  (lambda (ref)
    (and (integer? ref)
         (>= ref 0)
         (< ref (length the-almacen)))))

(define almacen-reservar!
  (lambda (v)
    (let ((ref (length the-almacen)))
      (begin
        (set! the-almacen (append the-almacen (list v)))
        ref))))

(define almacen-leer
  (lambda (ref)
    (if (referencia-valida? ref)
        (list-ref the-almacen ref)
        (eopl:error 'almacen-leer "Referencia invalida: ~s" ref))))

;; Devuelve una lista igual a lst, con v en la posición i.
(define reemplazar-en
  (lambda (lst i v)
    (if (= i 0)
        (cons v (cdr lst))
        (cons (car lst) (reemplazar-en (cdr lst) (- i 1) v)))))

(define almacen-escribir!
  (lambda (ref v)
    (if (referencia-valida? ref)
        (set! the-almacen (reemplazar-en the-almacen ref v))
        (eopl:error 'almacen-escribir! "Referencia invalida: ~s" ref))))
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

;; ---- ref, deref y set (Carlos) ----------------------------------------

;; Extrae el número de la referencia o aborta si el valor no es una referencia.
(define referencia-de
  (lambda (v quien)
    (cases valor v
      (valor-referencia (r) r)
      (else (eopl:error quien "~a no es una referencia" (valor->texto v))))))

;; ref(E): evalúa E, reserva una celda con ese valor y devuelve la referencia.
(define eval-ref
  (lambda (exp amb)
    (let ((v (value-of exp amb)))
      (valor-referencia (almacen-reservar! v)))))

;; Con el valor ya evaluado
(define deref-valor
  (lambda (vref)
    (almacen-leer (referencia-de vref 'deref))))

(define set-valor!
  (lambda (vref v)
    (begin
      (almacen-escribir! (referencia-de vref 'set) v)
      (valor-unit))))

;; deref(E): E debe evaluar a una referencia.
(define eval-deref
  (lambda (exp amb)
    (deref-valor (value-of exp amb))))

;; set E1 := E2: primero E1 y luego E2 (izquierda a derecha); devuelve unit.
(define eval-set
  (lambda (exp-ref exp-val amb)
    (let ((vref (value-of exp-ref amb)))
      (let ((v (value-of exp-val amb)))
        (set-valor! vref v)))))

;; ---- begin, if y while (Carlos) ---------------------------------------

;; Extrae el booleano o aborta. Lo usan if, while y las primitivas booleanas.
(define booleano-de
  (lambda (v quien)
    (cases valor v
      (valor-booleano (b) b)
      (else (eopl:error quien "se esperaba un booleano y llego ~a"
                        (valor->texto v))))))

;; begin: evalúa de izquierda a derecha y devuelve el último valor.
;; La gramática exige al menos una expresión.
(define eval-begin
  (lambda (exps amb)
    (let ((v (value-of (car exps) amb)))
      (if (null? (cdr exps))
          v
          (eval-begin (cdr exps) amb)))))

;; if / elif / else. pruebas y ramas son listas paralelas: la primera
;; prueba con su rama, luego cada elif con la suya. Solo se evalúa la rama
;; elegida, y cada prueba debe ser booleana.
(define eval-if
  (lambda (pruebas ramas sino amb)
    (if (null? pruebas)
        (value-of sino amb)
        (if (booleano-de (value-of (car pruebas) amb) 'if)
            (value-of (car ramas) amb)
            (eval-if (cdr pruebas) (cdr ramas) sino amb)))))

;; while: mientras la condición sea verdadera evalúa el cuerpo. Devuelve unit.
(define eval-while
  (lambda (prueba cuerpo amb)
    (if (booleano-de (value-of prueba amb) 'while)
        (begin
          (value-of cuerpo amb)
          (eval-while prueba cuerpo amb))
        (valor-unit))))


;; ---- Primitivas (Carlos) ----------------------------------------------

;; Reduce una lista de izquierda a derecha: (f (f a b) c)...
(define reducir
  (lambda (f lst)
    (letrec ((bucle (lambda (acc resto)
                      (if (null? resto)
                          acc
                          (bucle (f acc (car resto)) (cdr resto))))))
      (bucle (car lst) (cdr lst)))))

(define un-solo-argumento
  (lambda (op args)
    (if (= (length args) 1)
        (car args)
        (eopl:error op "requiere exactamente 1 operando y llegaron ~s"
                    (length args)))))

(define exactamente-dos
  (lambda (op args)
    (if (= (length args) 2)
        #t
        (eopl:error op "requiere exactamente 2 operandos y llegaron ~s"
                    (length args)))))

;; ---- numéricas ----

(define clase-numerica
  (lambda (v)
    (cases valor v
      (valor-entero (n) 'entero)
      (valor-flotante (x) 'flotante)
      (else #f))))

(define clase-de
  (lambda (op v)
    (let ((c (clase-numerica v)))
      (if c
          c
          (eopl:error op "~a no es un numero" (valor->texto v))))))

;; Todos los operandos deben ser de la misma clase numérica.
(define verificar-clase
  (lambda (op args clase)
    (cond ((null? args) clase)
          ((eqv? (clase-numerica (car args)) clase)
           (verificar-clase op (cdr args) clase))
          (else (eopl:error op
                  "los operandos deben ser numericos del mismo tipo y llego ~a"
                  (valor->texto (car args)))))))

(define numero-crudo
  (lambda (v)
    (cases valor v
      (valor-entero (n) n)
      (valor-flotante (x) x)
      (else (eopl:error 'numero-crudo "~a no es un numero" (valor->texto v))))))

(define construir-numero
  (lambda (clase n)
    (if (eqv? clase 'entero)
        (valor-entero n)
        (valor-flotante n))))

(define operar-dos
  (lambda (op clase a b)
    (cond ((eqv? op '+) (+ a b))
          ((eqv? op '-) (- a b))
          ((eqv? op '*) (* a b))
          ((eqv? op '/)
           (cond ((= b 0) (eopl:error '/ "Division por cero"))
                 ((eqv? clase 'entero) (quotient a b))
                 (else (/ a b))))
          (else ; %
           (cond ((not (eqv? clase 'entero))
                  (eopl:error '% "% solo acepta enteros"))
                 ((= b 0) (eopl:error '% "Division por cero"))
                 (else (remainder a b)))))))

(define operar-aritmetica
  (lambda (op args)
    (if (null? args)
        (eopl:error op "requiere al menos un operando")
        (let ((clase (verificar-clase op args (clase-de op (car args)))))
          (construir-numero
           clase
           (reducir (lambda (a b) (operar-dos op clase a b))
                    (map numero-crudo args)))))))

;; ---- relacionales ----

(define comparar-numeros
  (lambda (op args)
    (begin
      (exactamente-dos op args)
      (verificar-clase op args (clase-de op (car args)))
      (let ((a (numero-crudo (car args)))
            (b (numero-crudo (cadr args))))
        (valor-booleano
         (cond ((eqv? op '>) (> a b))
               ((eqv? op '<) (< a b))
               ((eqv? op '>=) (>= a b))
               (else (<= a b))))))))

;; == y != comparan valores de la misma clase: números, cadenas,
;; caracteres o booleanos.
(define comparable
  (lambda (op v)
    (cases valor v
      (valor-entero (n) (cons 'entero n))
      (valor-flotante (x) (cons 'flotante x))
      (valor-cadena (s) (cons 'cadena s))
      (valor-caracter (c) (cons 'caracter c))
      (valor-booleano (b) (cons 'booleano b))
      (else (eopl:error op "no se puede comparar ~a" (valor->texto v))))))

(define comparar-igualdad
  (lambda (op args)
    (begin
      (exactamente-dos op args)
      (let ((a (comparable op (car args)))
            (b (comparable op (cadr args))))
        (if (eqv? (car a) (car b))
            (valor-booleano (if (eqv? op '==)
                                (equal? (cdr a) (cdr b))
                                (not (equal? (cdr a) (cdr b)))))
            (eopl:error op "no se pueden comparar valores de distinto tipo: ~a y ~a"
                        (valor->texto (car args))
                        (valor->texto (cadr args))))))))

;; ---- booleanas ----

(define operar-booleana
  (lambda (op args)
    (cond ((eqv? op 'not)
           (valor-booleano (not (booleano-de (un-solo-argumento 'not args) 'not))))
          ((null? args) (eopl:error op "requiere al menos un operando"))
          (else
           (let ((bs (map (lambda (v) (booleano-de v op)) args)))
             (valor-booleano
              (if (eqv? op 'and)
                  (reducir (lambda (a b) (and a b)) bs)
                  (reducir (lambda (a b) (or a b)) bs))))))))

;; ---- cadenas, conversión y print ----

(define cadena-de
  (lambda (v quien)
    (cases valor v
      (valor-cadena (s) s)
      (else (eopl:error quien "se esperaba una cadena y llego ~a"
                        (valor->texto v))))))

(define texto->numero
  (lambda (s)
    (let ((n (string->number s)))
      (cond ((not n) (eopl:error 'to-number "~a no es un numero valido" s))
            ((and (integer? n) (exact? n)) (valor-entero n))
            ((and (real? n) (not (exact? n))) (valor-flotante n))
            (else (eopl:error 'to-number "~a no es un numero valido" s))))))

;; print escribe en la consola y devuelve unit. No es parte del almacén,
;; así que una reversión (entrega 2) no puede deshacerlo.
(define imprimir
  (lambda (v)
    (begin
      (eopl:printf "~a~%" (valor->texto v))
      (valor-unit))))

;; ---- despacho ----

(define aplicar-primitiva
  (lambda (op args)
    (cond ((memv op '(+ - * / %)) (operar-aritmetica op args))
          ((memv op '(> < >= <=)) (comparar-numeros op args))
          ((memv op '(== !=)) (comparar-igualdad op args))
          ((memv op '(and or not)) (operar-booleana op args))
          ((eqv? op 'concat)
           (if (null? args)
               (eopl:error 'concat "requiere al menos un operando")
               (valor-cadena
                (reducir string-append
                         (map (lambda (v) (cadena-de v 'concat)) args)))))
          ((eqv? op 'length)
           (valor-entero
            (string-length (cadena-de (un-solo-argumento 'length args) 'length))))
          ((eqv? op 'to-string)
           (valor-cadena (valor->texto (un-solo-argumento 'to-string args))))
          ((eqv? op 'to-number)
           (texto->numero (cadena-de (un-solo-argumento 'to-number args) 'to-number)))
          ((eqv? op 'print) (imprimir (un-solo-argumento 'print args)))
          (else (eopl:error 'aplicar-primitiva "primitiva desconocida ~s" op)))))

;; prim-exp: evalúa los operandos de izquierda a derecha y aplica.
(define eval-prim
  (lambda (op exps amb)
    (aplicar-primitiva op (evaluar-lista exps amb))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (inicializar-almacen!)
        (cases programa pgm
          (un-programa (cuerpo) (value-of cuerpo (ambiente-vacio))))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))