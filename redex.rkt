#lang racket
(require redex)
(provide (all-defined-out))

(define-language DOOF
  (e string
     x
     (λ (x t) e)
     (e e)
     (cat e e))
  (t str (-> t t))
  (x variable-not-otherwise-mentioned))

(define-extended-language DOOF+Γ DOOF
  [Γ · (x : t Γ)])

(define-judgment-form
  DOOF+Γ
  #:mode (types I I O)
  #:contract (types Γ e t)
  
  [--------------------
   (types Γ string str)]
  
  [---------------------
   (types (x : t Γ) x t)]
  
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : t_2 Γ) x_1 t_1)] 
  
  [(types (x : t_1 Γ) e t_2)
   ------------------------------------
   (types Γ (λ (x t_1) e) (-> t_1 t_2))]
  
  [(types Γ e_1 (-> t_2 t_3))
   (types Γ e_2 t_2)
   --------------------------
   (types Γ (e_1 e_2) t_3)]
  
  [(types Γ e_1 str)
   (types Γ e_2 str)
   ---------------------------
   (types Γ (cat e_1 e_2) str)])

(define-metafunction DOOF+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])

(define-extended-language Ev DOOF+Γ
  ; values
  (v string
     (λ (x t) e))
  ; contexts
  (E hole
     (E e)
     (v E)
     (cat E e)
     (cat v E)))

(define red
  (reduction-relation
   Ev
   #:domain e  
   (--> (in-hole E ((λ (x t) e) v))
        (in-hole E (subst x v e))
        "app")
   (--> (in-hole E (cat v_1 v_2))
        (in-hole E (str-cat v_1 v_2))
        "cat")))

(require redex/tut-subst)
(define-metafunction Ev
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
(define x? (redex-match Ev x))

(define-metafunction Ev
  str-cat : string string -> string
  [(str-cat string_1 string_2)
   ,(string-append (term string_1) (term string_2))])



