#lang racket
(require redex)
(provide (all-defined-out))

(require "lang.rkt" "pat.rkt")

(define-extended-language doof-kc doof
  (Γk • (X : k Γk)))

; Subtyping relation
(define-relation doof
  <: ⊆ t × t
  ; S-Arrow
  [(<: (-> t_11 t_12) (-> t_21 t_22))
   (<: t_21 t_11)
   (<: t_12 t_22)]
  ; S-Pat
  [(<: p_1 p_2)
   (<p p_1 p_2)]
  ; S-Obj
  [(<: (t-obj (string_n1 t_v1) ...) (t-obj (string_n2 t_v2) ...))
   (side-condition
    (andmap
     (λ (n2 v2)
       (ormap
        (λ (n1 v1)
          (and (equal? n1 n2)
               (term (<: ,v1 ,v2))))
        (term (string_n1 ...))
        (term (t_v1 ...))))
     (term (string_n2 ...))
     (term (t_v2 ...))))])

; Kind checking
(define-judgment-form doof-kc
  #:mode (kinds I I O)
  #:contract (kinds Γk t k)
  
  [(kinds (X : k Γk) X k) "k-var"]
  
  [(kinds Γk X_1 k_1)
   (side-condition (distinct X_1 X_2))
   ----------------------------------- "k-ctx"
   (kinds (X_2 : t_2 Γk) X_1 k_1)]
  
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
   (<: t_1 str)
   (<: t_2 str)
   ---------------------------- "k-cat"
   (kinds Γk (t-cat t_1 t_2) *)]
  
  [(kinds Γk (t-obj (string t) ...) *) "k-obj"])
