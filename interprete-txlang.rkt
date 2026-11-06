#lang eopl

;; ==========================================================
;; Fachada principal del intérprete TxLang-UV

;; Este módulo conecta y exporta los componentes implementados
;; en src para que las baterías de pruebas puedan utilizarlos.

(require "src/sintaxis.rkt"
         "src/unparse.rkt"
         "src/ambiente.rkt"
         "src/almacen.rkt"
         "src/valores.rkt"
         "src/tipos.rkt"
         "src/interprete.rkt"
         "src/interfaz.rkt")

(provide
 (all-from-out "src/sintaxis.rkt")
 (all-from-out "src/unparse.rkt")
 (all-from-out "src/ambiente.rkt")
 (all-from-out "src/almacen.rkt")
 (all-from-out "src/valores.rkt")
 (all-from-out "src/tipos.rkt")
 (all-from-out "src/interprete.rkt")
 (all-from-out "src/interfaz.rkt"))