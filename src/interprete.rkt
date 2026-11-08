#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2, Nombre3 Codigo3

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
      (else (eopl:error 'value-of "construcción aún no implementada")))))