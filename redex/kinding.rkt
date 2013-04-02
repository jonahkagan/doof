#lang racket
(require redex)
(provide (all-defined-out))

(require "lang.rkt" "pat.rkt")

(define-extended-language doof-kc doof
  (Γk • (X : k Γk)))

; Kind checking
(define-judgment-form doof-kc
  #:mode (kinds I I O)
  #:contract (kinds Γk t k)
  
  [(kinds (X : k Γk) X k) "k-var"]
  
  [(kinds Γk X_1 k_1)
   (side-condition (distinct X_1 X_2))
   ----------------------------------- "k-ctx"
   (kinds (X_2 : k_2 Γk) X_1 k_1)]
  
  [(kinds (X : k_1 Γk) t k_2)
   -------------------------------------- "k-abs"
   (kinds Γk (tλ (X k_1) t) (=> k_1 k_2))]
  
  [(kinds Γk t_1 (=> k_11 k_12))
   (kinds Γk t_2 k_11)
   ----------------------------- "k-app"
   (kinds Γk (t_1 t_2) k_12)]
  
  [(kinds Γk t_1 *)
   (kinds Γk t_2 *)
   ------------------------- "k-arrow"
   (kinds Γk (-> t_1 t_2) *)]
  
  [(kinds Γk p *) "k-pat"]
  
  [(kinds Γk t_1 *)
   (kinds Γk t_2 *)
   ;(<: t_1 str)
   ;(<: t_2 str)
   ---------------------------- "k-cat"
   (kinds Γk (t-cat t_1 t_2) *)]
  
  [(kinds Γk (t-obj (string t) ...) *) "k-obj"]
  
  [(kinds Γk t_1 *)
   (kinds Γk t_2 *)
   (kinds Γk t_3 *)
   ;(<: t_1 (t-obj))
   ;(<: t_2 str)
   -------------------------------- "k-ext"
   (kinds Γk (t-ext t_1 t_2 t_3) *)]
  
  [(kinds Γk t_1 (=> * (=> * (=> * *))))
   (kinds Γk t_2 *)
   (kinds Γk t_3 *)
   ;(<: t_3 (t-obj))
   ------------------------------------- "k-fold"
   (kinds Γk (t-fold t_1 t_2 t_3) *)])
