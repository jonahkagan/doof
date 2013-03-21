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
     (cat e e)
     (obj (string e) ...)
     (ext e e e)
     (get e e))
  (x variable-not-otherwise-mentioned)
  ; Types
  (t p
     (-> t t)
     (t-obj (p t) ...)))

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
   (<p p_1 p_2)]
  ; S-ObjObj
  [(<: (t-obj (string_n1 t_v1) ...) (t-obj (string_n2 t_v2) ...))
   (side-condition
    (andmap
     (λ (f1)
       (andmap
        (λ (f2)
          (if (equal? (car f1) (car f2))
              (term (<: ,(cdr f1) ,(cdr f2)))
              #t))
        (map cons
             (term (string_n2 ...))
             (term (t_v2 ...)))))
     (map cons
          (term (string_n1 ...))
          (term (t_v1 ...)))))]
  )

(define-extended-language doof-tc doof
  [Γ · (x : t Γ)])

; Type checking
(define-judgment-form doof-tc
  #:mode (types I I O)
  #:contract (types Γ e t)
  
  [(types Γ string_1 string_1) "t-str"]
  
  [(types (x : t Γ) x t) "t-var"]
  
  [(types Γ x_1 t_1)
   (side-condition (distinct x_1 x_2))
   ------------------------------------ "t-ctx"
   (types (x_2 : t_2 Γ) x_1 t_1)]
  
  [(types (x : t_1 Γ) e t_2)
   ------------------------------------ "t-abs"
   (types Γ (λ (x t_1) e) (-> t_1 t_2))]
  
  [(types Γ e_1 (-> t_11 t_12))
   (types Γ e_2 t_2)
   (<: t_2 t_11)
   ---------------------------- "t-app"
   (types Γ (e_1 e_2) t_12)]
  
  [(types Γ e_1 t_1)
   (types Γ e_2 t_2)
   (<: t_1 str)
   (<: t_2 str)
   --------------------------------------- "t-cat"
   (types Γ (cat e_1 e_2) (p-cat t_1 t_2))]
  
  [(types Γ e_1 t_1) ...
   (side-condition (distinct (string_1 ...)))
   ------------------------------------------------ "t-obj"
   (types Γ (obj (string_1 e_1) ...)
          (t-obj (string_1 t_1) ...))]
  
  [(types Γ e_1 (t-obj (string_1 t_1) ...))
   (types Γ e_2 string_new)
   (types Γ e_3 t_new)
   (side-condition ,(not (member (term string_new)
                                 (term (string_1 ...)))))
   ----------------------------------------------------- "t-ext-add"
   (types Γ (ext e_1 e_2 e_3)
          (t-obj (string_new t_new) (string_1 t_1) ...))]
  
  [(types Γ e_1 (t-obj (string_1 t_1) ...
                       (string_k t_k)
                       (string_k+1 t_k+1) ...))
   (types Γ e_2 string_k)
   (types Γ e_3 t_new)
   -------------------------------------------- "t-ext-replace"
   (types Γ (ext e_1 e_2 e_3)
          (t-obj (string_1 t_1) ...
                 (string_k t_new)
                 (string_k+1 t_k+1) ...))]
  
  [(types Γ e_1 (t-obj (string_1 t_1) ...
                       (string_k t_k)
                       (string_k+1 t_k+1) ...))
   (types Γ e_2 string_k)
   -------------------------------------------- "t-get"
   (types Γ (get e_1 e_2) t_k)]
  )

(define-metafunction doof-tc
  distinct : any ... -> boolean
  [(distinct any_1 ...)
   ,(let ([as (term (any_1 ...))]) 
     (= (length as)
        (length (remove-duplicates as))))])

(define-extended-language doof-ctx doof-tc
  ; Values
  (v string
     (obj (string v) ...)
     (λ (x t) e))
  ; Contexts
  (E hole
     (E e)
     (v E)
     (cat E e)
     (cat v E)
     (obj (string v) ... (string E) (string e) ...)
     (ext E e e)
     (ext v E e)
     (ext v v E)
     (get E e)
     (get v E)))

(define red
  (reduction-relation
   doof-ctx
   ;#:domain e  
   (==> ((λ (x t) e) v) (subst x v e)
        "e-app")
   (==> (cat v_1 v_2) (str-cat v_1 v_2)
        "e-cat")
   (==> (ext (obj (string_1 v_1) ...) string_new v_new)
        (obj (string_new v_new) (string_1 v_1) ...)
        (side-condition (not (member (term string_new)
                                     (term (string_1 ...)))))
        "e-ext-add")
   (==> (ext (obj (string_1 v_1) ... (string_k v_k) (string_k+1 v_k+1) ...)
             string_k v_new)
        (obj (string_1 v_1) ... (string_k v_new) (string_k+1 v_k+1) ...)
        "e-ext-replace")
   (==> (get (obj (string_1 v_1) ... (string_k v_k) (string_k+1 v_k+1) ...)
             string_k)
        v_k
        "e-get")
   
   with
   [(--> (in-hole E e_1) (in-hole E e_2))
    (==> e_1 e_2)]
   ))

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



