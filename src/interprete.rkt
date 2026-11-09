#lang eopl
;Autores: Oliver De Jesus Arboleda Baez 2459684, Maria Fernanda Betancourt Montoya 2459510, Kevin Andres Rosero Romo 2459554

(require "sintaxis.rkt")
(require "ambiente.rkt")
(require "almacen.rkt")
(require "valores.rkt")


;; =========================================================
;; MODULO: INTERPRETE
;; Evaluacion de expresiones TxLang-UV.
;; Alcance: primera entrega.
;; =========================================================


;; =========================================================
;; 1. Evaluacion de listas y secuencias

(define evaluar-lista
  (lambda (expresiones amb)
    (if (null? expresiones)
        '()
        (let ((primero (value-of (car expresiones) amb)))
          (cons primero
                (evaluar-lista (cdr expresiones) amb))))))


(define evaluar-secuencia
  (lambda (expresiones amb)
    (if (null? (cdr expresiones))
        (value-of (car expresiones) amb)
        (begin
          (value-of (car expresiones) amb)
          (evaluar-secuencia (cdr expresiones) amb)))))


;; ---------------------------------------------------------
;; 2. Verificacion de condiciones booleanas

(define prueba->bool
  (lambda (valor quien)
    (expval->bool valor)))


;; ---------------------------------------------------------
;; 3. Evaluacion de condicionales elif

(define evaluar-elifs
  (lambda (pruebas ramas rama-else amb)
    (cond
      ((null? pruebas)
       (value-of rama-else amb))

      ((prueba->bool
        (value-of (car pruebas) amb)
        'elif)
       (value-of (car ramas) amb))

      (else
       (evaluar-elifs
        (cdr pruebas)
        (cdr ramas)
        rama-else
        amb)))))


;; ---------------------------------------------------------
;; 4. Ciclo while

(define ejecutar-while
  (lambda (prueba cuerpo amb)
    (if (prueba->bool (value-of prueba amb) 'while)
        (begin
          (value-of cuerpo amb)
          (ejecutar-while prueba cuerpo amb))
        (unit-val))))


;; ---------------------------------------------------------
;; 5. Aplicacion de procedimientos

(define aplicar-proc
  (lambda (funcion argumentos)
    (cases expval funcion
      (proc-val (parametros cuerpo amb-clausura)
        (if (= (length parametros) (length argumentos))
            (value-of
             cuerpo
             (extender-amb parametros argumentos amb-clausura))
            (eopl:error
             'apply
             "Cantidad incorrecta de argumentos. Esperados: ~s; recibidos: ~s"
             (length parametros)
             (length argumentos))))

      (else
       (eopl:error
        'apply
        "Se esperaba un procedimiento, se recibio: ~s"
        funcion)))))


;; ---------------------------------------------------------
;; 6. Verificacion de aridad

(define verificar-aridad
  (lambda (nombre argumentos cantidad)
    (if (= (length argumentos) cantidad)
        #t
        (eopl:error
         nombre
         "Se esperaban ~s argumentos y llegaron ~s"
         cantidad
         (length argumentos)))))


;; ---------------------------------------------------------
;; 7. Operaciones aritmeticas

(define aritmetica
  (lambda (nombre operacion argumentos)
    (begin
      (verificar-aridad nombre argumentos 2)

      (let ((a (expval->num (car argumentos)))
            (b (expval->num (cadr argumentos))))

        (if (and (eq? nombre '/) (= b 0))
            (eopl:error nombre "Division por cero")
            (num-val (operacion a b)))))))


;; ---------------------------------------------------------
;; 8. Operaciones relacionales

(define comparar
  (lambda (nombre operacion argumentos)
    (begin
      (verificar-aridad nombre argumentos 2)

      (let ((a (expval->num (car argumentos)))
            (b (expval->num (cadr argumentos))))

        (bool-val (operacion a b))))))


;; ---------------------------------------------------------
;; 9. Igualdad

(define valores-iguales?
  (lambda (a b)
    (cases expval a

      (num-val (n)
        (cases expval b
          (num-val (m) (= n m))
          (else #f)))

      (bool-val (n)
        (cases expval b
          (bool-val (m) (eq? n m))
          (else #f)))

      (string-val (n)
        (cases expval b
          (string-val (m) (string=? n m))
          (else #f)))

      (char-val (n)
        (cases expval b
          (char-val (m) (char=? n m))
          (else #f)))

      (ref-val (n)
        (cases expval b
          (ref-val (m) (= n m))
          (else #f)))

      (unit-val ()
        (cases expval b
          (unit-val () #t)
          (else #f)))

      (proc-val (p c e)
        #f))))


(define igualdad
  (lambda (argumentos negar?)
    (begin
      (verificar-aridad 'igualdad argumentos 2)

      (let ((resultado
             (valores-iguales?
              (car argumentos)
              (cadr argumentos))))

        (bool-val
         (if negar?
             (not resultado)
             resultado))))))


;; ---------------------------------------------------------
;; 10. Primitivas

(define aplicar-primitiva
  (lambda (operador argumentos)

    (cases primitiva operador

      (suma-prim ()
        (aritmetica '+ + argumentos))

      (resta-prim ()
        (aritmetica '- - argumentos))

      (mult-prim ()
        (aritmetica '* * argumentos))

      (div-prim ()
        (aritmetica '/ / argumentos))

      (mod-prim ()
        (begin
          (verificar-aridad '% argumentos 2)
          (let ((a (expval->num (car argumentos)))
                (b (expval->num (cadr argumentos))))
            (if (or (not (integer? a))
                    (not (integer? b)))
                (eopl:error '% "Se requieren enteros")
                (if (= b 0)
                    (eopl:error '% "Division por cero")
                    (num-val (remainder a b)))))))

      (mayor-prim ()
        (comparar '> > argumentos))

      (menor-prim ()
        (comparar '< < argumentos))

      (mayor-igual-prim ()
        (comparar '>= >= argumentos))

      (menor-igual-prim ()
        (comparar '<= <= argumentos))

      (igual-prim ()
        (igualdad argumentos #f))

      (diferente-prim ()
        (igualdad argumentos #t))

      (and-prim ()
        (begin
          (verificar-aridad 'and argumentos 2)
          (bool-val
           (and
            (expval->bool (car argumentos))
            (expval->bool (cadr argumentos))))))

      (or-prim ()
        (begin
          (verificar-aridad 'or argumentos 2)
          (bool-val
           (or
            (expval->bool (car argumentos))
            (expval->bool (cadr argumentos))))))

      (not-prim ()
        (begin
          (verificar-aridad 'not argumentos 1)
          (bool-val
           (not (expval->bool (car argumentos))))))

      (concat-prim ()
        (begin
          (verificar-aridad 'concat argumentos 2)
          (string-val
           (string-append
            (expval->string (car argumentos))
            (expval->string (cadr argumentos))))))

      (length-prim ()
        (begin
          (verificar-aridad 'length argumentos 1)
          (num-val
           (string-length
            (expval->string (car argumentos))))))

      (to-string-prim ()
        (begin
          (verificar-aridad 'to-string argumentos 1)
          (string-val
           (expval->texto (car argumentos)))))

      (to-number-prim ()
        (begin
          (verificar-aridad 'to-number argumentos 1)
          (let ((resultado
                 (string->number
                  (expval->string (car argumentos)))))
            (if resultado
                (num-val resultado)
                (eopl:error
                 'to-number
                 "No se puede convertir el texto a numero")))))

      (print-prim ()
        (begin
          (verificar-aridad 'print argumentos 1)
          (display (expval->texto (car argumentos)))
          (newline)
          (unit-val))))))

(define buscar-valor-amb
  (lambda (amb id)
    (let ((encontrado (buscar-amb amb id)))
      (if (enlace-recursivo? encontrado)
          (cases enlace-recursivo encontrado
            (enlace-proc-rec (ids cuerpo amb-rec)
              (proc-val ids cuerpo amb-rec)))
          encontrado))))


;; ---------------------------------------------------------
;; 11. Evaluacion de expresiones: value-of

(define value-of
  (lambda (exp amb)

    (cases expresion exp

      ;; Literales
      (lit-exp (n)
        (num-val n))

      (flot-exp (n)
        (num-val n))

      (cadena-exp (c)
        (string-val (texto-de-cadena c)))

      (car-exp (c)
        (char-val (caracter-de-token c)))

      (true-exp ()
        (bool-val #t))

      (false-exp ()
        (bool-val #f))

      (unit-exp ()
        (unit-val))


      ;; Variables
      (var-exp (id)
  (buscar-valor-amb amb id))


      ;; Ligaduras let
      (let-exp (ids exps cuerpo)
        (value-of
         cuerpo
         (extender-amb
          ids
          (evaluar-lista exps amb)
          amb)))


      ;; Referencias
      (ref-exp (e)
        (ref-val
         (nuevo-ref (value-of e amb))))

      (deref-exp (e)
        (obtener-ref
         (expval->ref (value-of e amb))))

      (set-exp (e1 e2)
        (let ((referencia
               (expval->ref (value-of e1 amb)))
              (valor
               (value-of e2 amb)))
          (begin
            (actualizar-ref! referencia valor)
            (unit-val))))


      ;; Secuenciacion
      (begin-exp (primera resto)
        (evaluar-secuencia
         (cons primera resto)
         amb))


      ;; Condicional
      (if-exp (prueba rama-then elif-pruebas elif-ramas rama-else)
        (if (prueba->bool (value-of prueba amb) 'if)
            (value-of rama-then amb)
            (evaluar-elifs
             elif-pruebas
             elif-ramas
             rama-else
             amb)))


      ;; Ciclo
      (while-exp (prueba cuerpo)
        (ejecutar-while prueba cuerpo amb))


      ;; Primitivas
      (prim-exp (prim argumentos)
        (aplicar-primitiva
         prim
         (evaluar-lista argumentos amb)))


      ;; Procedimientos
      (proc-exp (ids tipos cuerpo)
        (proc-val ids cuerpo amb))


      ;; Aplicacion
      (apply-exp (rator rands)
        (let ((funcion (value-of rator amb))
              (argumentos (evaluar-lista rands amb)))
          (aplicar-proc funcion argumentos)))


      ;; Recursion
      (letrec-exp (tipos-ret nombres ids-lista tipos-lista cuerpos cuerpo)
        (value-of
         cuerpo
         (extender-amb-rec
          nombres
          ids-lista
          cuerpos
          amb)))


      ;; Construcciones de segunda entrega
      (atomic-exp (cuerpo manejador-id manejador)
        (eopl:error
         'atomic
         "Los bloques atomic pertenecen a la segunda entrega"))

      (abort-exp (e)
        (eopl:error
         'abort
         "abort pertenece a la segunda entrega"))

      (new-exp (nombre campos valores)
        (eopl:error
         'new
         "Los registros pertenecen a la segunda entrega"))

      (field-exp (e campo)
        (eopl:error
         'field
         "Los registros pertenecen a la segunda entrega")))))


(provide (all-defined-out))

(define probar-interprete
  (lambda (codigo)
    (let ((arbol (scan&parse codigo)))
      (cases programa arbol
        (un-programa (declaraciones expresion)
          (begin
            (inicializar-almacen!)
            (value-of expresion (amb-vacio))))))))
