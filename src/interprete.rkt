#lang eopl
;Autores: Samuel Garcia Parra 2459476, Juan Sebastian Navarrete Rada 2459562

(require "sintaxis.rkt" "ambiente.rkt" "almacen.rkt" "valores.rkt")
(provide (all-defined-out))

;; Los tokens cadena y caracter traen sus comillas.
(define quitar-comillas
  (lambda (s) (substring s 1 (- (string-length s) 1))))


;; evaluar-lista : listaDeExpresion x ambiente -> listaDeValor
(define evaluar-lista
  (lambda (exps amb)
    (map (lambda (e) (value-of e amb)) exps)))

;; evaluar-secuencia : expresion x listaDeExpresion x ambiente -> valor
;; evalúa de izquierda a derecha y devuelve el último
(define evaluar-secuencia
  (lambda (primera resto amb)
    (let ((v (value-of primera amb)))
      (if (null? resto)
          v
          (evaluar-secuencia (car resto) (cdr resto) amb)))))

;; evaluar-if : evalúa la prueba y elige la rama
(define evaluar-if
  (lambda (c t conds exps e amb)
    (let ((p (value-of c amb)))
      (cases valor p
        (bool-val (b)
          (cond (b (value-of t amb))
                ((null? conds) (value-of e amb))
                (else (evaluar-if (car conds) (car exps)
                                  (cdr conds) (cdr exps) e amb))))
        (else (eopl:error 'if "la prueba debe ser booleana y llegó ~s"
                          (valor->texto p)))))))

;; evaluar-while : repite el cuerpo mientras la condición sea verdadera
(define evaluar-while
  (lambda (c cuerpo amb)
    (let ((p (value-of c amb)))
      (cases valor p
        (bool-val (b)
          (if b
              (begin (value-of cuerpo amb)
                     (evaluar-while c cuerpo amb))
              (unit-val)))
        (else (eopl:error 'while "la condición debe ser booleana y llegó ~s"
                          (valor->texto p)))))))

;; value-of : expresion x ambiente -> valor
;; Por ahora solo literales; el resto lo agrega el grupo.
;; --- Persona 2: primitivas y procedimientos ---
(define verificar-aridad
  (lambda (nombre n args)
    (if (= (length args) n)
        #t
        (eopl:error nombre "se esperaban ~s argumentos y llegaron ~s"
                    n (length args)))))

(define error-numerico
  (lambda (nombre a b)
    (eopl:error nombre "se esperaban dos numeros del mismo tipo y llegaron ~s y ~s"
                (valor->texto a) (valor->texto b))))

;; con-numeros : dos int-val o dos float-val; mezclar tipos es error
(define con-numeros
  (lambda (nombre args f-ent f-flot)
    (begin
      (verificar-aridad nombre 2 args)
      (let ((a (car args)) (b (cadr args)))
        (cases valor a
          (int-val (x)
            (cases valor b
              (int-val (y) (f-ent x y))
              (else (error-numerico nombre a b))))
          (float-val (x)
            (cases valor b
              (float-val (y) (f-flot x y))
              (else (error-numerico nombre a b))))
          (else (error-numerico nombre a b)))))))

(define aritmetica
  (lambda (nombre op args)
    (con-numeros nombre args
                 (lambda (x y) (int-val (op x y)))
                 (lambda (x y) (float-val (op x y))))))

(define comparar
  (lambda (nombre op args)
    (con-numeros nombre args
                 (lambda (x y) (bool-val (op x y)))
                 (lambda (x y) (bool-val (op x y))))))

(define dividir
  (lambda (args)
    (con-numeros 'div args
      (lambda (x y) (if (= y 0)
                        (eopl:error 'div "division por cero")
                        (int-val (quotient x y))))
      (lambda (x y) (if (= y 0.0)
                        (eopl:error 'div "division por cero")
                        (float-val (/ x y)))))))

(define resto
  (lambda (args)
    (begin
      (verificar-aridad 'mod 2 args)
      (let ((a (car args)) (b (cadr args)))
        (cases valor a
          (int-val (x)
            (cases valor b
              (int-val (y) (if (= y 0)
                               (eopl:error 'mod "division por cero")
                               (int-val (remainder x y))))
              (else (eopl:error 'mod "% solo acepta enteros y llegó ~s"
                                (valor->texto b)))))
          (else (eopl:error 'mod "% solo acepta enteros y llegó ~s"
                            (valor->texto a))))))))

(define valores-iguales?
  (lambda (a b)
    (cases valor a
      (int-val (x)   (cases valor b (int-val (y) (= x y)) (else #f)))
      (float-val (x) (cases valor b (float-val (y) (= x y)) (else #f)))
      (str-val (x)   (cases valor b (str-val (y) (string=? x y)) (else #f)))
      (char-val (x)  (cases valor b (char-val (y) (char=? x y)) (else #f)))
      (bool-val (x)  (cases valor b (bool-val (y) (eq? x y)) (else #f)))
      (unit-val ()   (cases valor b (unit-val () #t) (else #f)))
      (ref-val (x)   (cases valor b (ref-val (y) (= x y)) (else #f)))
      (else (eopl:error '== "no se comparan procedimientos ni registros")))))

(define a-numero
  (lambda (v)
    (let ((n (string->number (valor->string v))))
      (cond ((not n) (eopl:error 'to-number "no se puede convertir ~s" (valor->string v)))
            ((and (exact? n) (integer? n)) (int-val n))
            ((real? n) (float-val (exact->inexact n)))
            (else (eopl:error 'to-number "no se puede convertir ~s" (valor->string v)))))))

(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim
      (suma-prim ()  (aritmetica 'suma + args))
      (resta-prim () (aritmetica 'resta - args))
      (mult-prim ()  (aritmetica 'mult * args))
      (div-prim ()   (dividir args))
      (mod-prim ()   (resto args))
      (mayor-prim ()       (comparar 'mayor > args))
      (menor-prim ()       (comparar 'menor < args))
      (mayor-igual-prim () (comparar 'mayor-igual >= args))
      (menor-igual-prim () (comparar 'menor-igual <= args))
      (igual-prim ()     (begin (verificar-aridad '== 2 args)
                                (bool-val (valores-iguales? (car args) (cadr args)))))
      (diferente-prim () (begin (verificar-aridad '!= 2 args)
                                (bool-val (not (valores-iguales? (car args) (cadr args))))))
      (and-prim () (begin (verificar-aridad 'and 2 args)
                          (let ((x (valor->bool (car args))) (y (valor->bool (cadr args))))
                            (bool-val (and x y)))))
      (or-prim ()  (begin (verificar-aridad 'or 2 args)
                          (let ((x (valor->bool (car args))) (y (valor->bool (cadr args))))
                            (bool-val (or x y)))))
      (not-prim () (begin (verificar-aridad 'not 1 args)
                          (bool-val (not (valor->bool (car args))))))
      (concat-prim () (begin (verificar-aridad 'concat 2 args)
                             (str-val (string-append (valor->string (car args))
                                                     (valor->string (cadr args))))))
      (length-prim () (begin (verificar-aridad 'length 1 args)
                             (int-val (string-length (valor->string (car args))))))
      (to-string-prim ()  (begin (verificar-aridad 'to-string 1 args)
                                 (str-val (valor->texto (car args)))))
      (to-number-prim ()  (begin (verificar-aridad 'to-number 1 args)
                                 (a-numero (car args))))
      (print-prim () (begin (verificar-aridad 'print 1 args)
                            (display (valor->texto (car args)))
                            (newline)
                            (unit-val))))))

(define aplicar-proc
  (lambda (f args)
    (cases valor f
      (proc-val (ids cuerpo amb-clausura)
        (if (= (length ids) (length args))
            (value-of cuerpo (extend-env ids args amb-clausura))
            (eopl:error 'apply "se esperaban ~s argumentos y llegaron ~s"
                        (length ids) (length args))))
      (else (eopl:error 'apply "se esperaba un procedimiento y llegó ~s"
                        (valor->texto f))))))

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) (int-val n))
      (flotante-exp (n) (float-val n))
      (cadena-exp (s) (str-val (quitar-comillas s)))
      (caracter-exp (c) (char-val (string-ref c 1)))
      (true-exp () (bool-val #t))
      (false-exp () (bool-val #f))
      (unit-exp () (unit-val))
      (var-exp (id) (apply-env amb id))
 
      ;; --- Persona 1: let, begin, if, while ---
      (let-exp (ids exps cuerpo)
        (let ((vals (evaluar-lista exps amb)))
          (value-of cuerpo (extend-env ids vals amb))))
      (begin-exp (primera resto)
        (evaluar-secuencia primera resto amb))
      (if-exp (c t conds exps e)
        (evaluar-if c t conds exps e amb))
      (while-exp (c cuerpo)
        (evaluar-while c cuerpo amb))
 
      ;; --- Persona 2: primitivas, proc, apply, letrec ---
      (prim-exp (prim rands)
        (apply-primitive prim (evaluar-lista rands amb)))
      (proc-exp (ids tipos cuerpo)
        (proc-val ids cuerpo amb))
      (apply-exp (rator rands)
        (let* ((f (value-of rator amb))
               (args (evaluar-lista rands amb)))
          (aplicar-proc f args)))
      (letrec-exp (rtipos nombres idss tiposs cuerpos cuerpo)
        (value-of cuerpo (extend-env-rec nombres idss cuerpos amb)))
 
      ;; --- ref, deref, set ---
      (ref-exp (e)
        (ref-val (newref! (value-of e amb))))
      (deref-exp (e)
        (deref-ref (valor->ref (value-of e amb))))
      (set-exp (e1 e2)
        (let* ((r (valor->ref (value-of e1 amb)))
               (v (value-of e2 amb)))
          (begin (setref! r v)
                 (unit-val))))
 
      ;; atomic, abort, new y field llegan en la segunda entrega.
      (else (eopl:error 'value-of "construcción aún no implementada")))))