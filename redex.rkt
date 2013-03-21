#lang racket
(require redex)
(provide (all-defined-out))

; String patterns
(define-language pat
  (p str ; the set of all strings
     string
     (p-cat p p)))

(define-extended-language doof pat
  ; Expressions
  (e string
     x
     (λ (x t) e)
     (e e)
     (cat e e))
  ;mt-obj
  ;(ext e e e)
  ;(get e e))
  ; Types
  (t p
     (-> t t))
  ;t-mt-obj
  ;(t-ext t t t)
  ;(t-get t t t))
  
  (x variable-not-otherwise-mentioned))

; Sub-pattern relation
(define-relation pat
  <p ⊆ p × p
  [(<p p p)]
  [(<p string str)]
  [(<p (p-cat p p) str)]
  [(<p (p-cat p_1 p_2) p_3)
   (where p_3 (pat-reduce (p-cat p_1 p_2)))]
  [(<p p_1 (p-cat p_2 p_3))
   (where p_1 (pat-reduce (p-cat p_2 p_3)))])

; might want to make this a reduction relation...
(define-metafunction pat
  pat-reduce : p -> p
  [(pat-reduce (p-cat string_1 string_2))
   ,(string-append (term string_1) (term string_2))]
  [(pat-reduce (p-cat p_1 p_2))
   (p-cat (pat-reduce p_1) p_2)
   (side-condition (not (string? (term p_1))))]
  [(pat-reduce (p-cat string_1 p_2))
   (p-cat string_1 (pat-reduce p_2))
   (side-condition (not (string? (term p_2))))]
  [(pat-reduce p) p])

; Subtyping relation
(define-relation doof
  <: ⊆ t × t
  ; S-Arrow
  [(<: (-> t_11 t_12) (-> t_21 t_22))
   (<: t_21 t_11)
   (<: t_12 t_22)]
  ; S-Pat
  [(<: p_1 p_2)
   (<p p_1 p_2)])

(define-extended-language doof-tc doof
  [Γ · (x : t Γ)])

; Type checking
(define-judgment-form doof-tc
  #:mode (types I I O)
  #:contract (types Γ e t)
  
  [(types Γ string_1 string_1) "t-str"]
  
  [(types (x : t Γ) x t) "t-var"]
  
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------ "t-ctx"
   (types (x_2 : t_2 Γ) x_1 t_1)]
  
  [(types (x : t_1 Γ) e t_2)
   ------------------------------------ "t-abs"
   (types Γ (λ (x t_1) e) (-> t_1 t_2))]
  
  [(types Γ e_1 (-> t_11 t_12))
   (types Γ e_2 t_2)
   (<: t_2 t_11)
   -------------------------- "t-app"
   (types Γ (e_1 e_2) t_12)]
  
  [(types Γ e_1 t_1)
   (types Γ e_2 t_2)
   (<: t_1 str)
   (<: t_2 str)
   --------------------------------------- "p-cat"
   (types Γ (cat e_1 e_2) (p-cat t_1 t_2))]
  )

(define-metafunction doof-tc
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])

(define-extended-language doof-ctx doof-tc
  ; Values
  (v string
     (λ (x t) e))
  ; Contexts
  (E hole
     (E e)
     (v E)
     (cat E e)
     (cat v E)))

(define red
  (reduction-relation
   doof-ctx
   #:domain e  
   (--> (in-hole E ((λ (x t) e) v))
        (in-hole E (subst x v e))
        "app")
   (--> (in-hole E (cat v_1 v_2))
        (in-hole E (str-cat v_1 v_2))
        "cat")))

(require redex/tut-subst)
(define-metafunction doof-ctx
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
(define x? (redex-match doof-ctx x))

(define-metafunction doof-ctx
  str-cat : string string -> string
  [(str-cat string_1 string_2)
   ,(string-append (term string_1) (term string_2))])



