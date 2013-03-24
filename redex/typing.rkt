#lang racket
(require redex)
(provide (all-defined-out))

(require "pat.rkt"
         "lang.rkt"
         "kinding.rkt")

(define-extended-language doof-tc doof-kc
  (Γ • (x : t Γ))
  (tv p
      (-> t t)
      (t-obj (string t) ...)
      (tλ (X k) t))
  (tE hole
      (tE t)
      (tv tE)
      (-> tE t)
      (-> tv tE)
      (t-cat tE t)
      (t-cat tv tE)
      (t-obj (string tv) ... (string tE) (string t) ...)
      (t-ext tE t t)
      (t-ext tv tE t)
      (t-ext tv tv tE)))

; Type (parallel) reduction
(define t-red
  (reduction-relation
   doof-tc
   #:domain t
   
   (==> ((tλ (X k) t) tv) (t-subst X tv t)
        "q-app")
   (==> (t-cat p_1 p_2) (p-cat p_1 p_2)
        "q-cat")
   (==> p p_2
        (where p_2 (pat-reduce p))
        (side-condition (not (equal? (term p) (term p_2))))
        "q-pat")
   (==> (t-ext (t-obj (string_1 tv_1) ...) string_new tv_new)
        (t-obj (string_new tv_new) (string_1 tv_1) ...)
        (side-condition (not (member (term string_new)
                                     (term (string_1 ...)))))
        "q-ext-add")
   (==> (t-ext (t-obj (string_1 tv_1) ... (string_k tv_k) (string_k+1 tv_k+1) ...)
               string_k tv_new)
        (t-obj (string_1 tv_1) ... (string_k tv_new) (string_k+1 tv_k+1) ...)
        "q-ext-replace")
   
   with
   [(--> (in-hole tE t_1) (in-hole tE t_2))
    (==> t_1 t_2)]
   ))

(require redex/tut-subst)
(define-metafunction doof-tc
  t-subst : X tv t -> t
  [(t-subst X tv t)
   ,(subst/proc X? (list (term X)) (list (term tv)) (term t))])
(define X? (redex-match doof X))

(define-metafunction doof-tc
  t-reduce : t -> tv
  [(t-reduce t)
   ,(let ([tvs (apply-reduction-relation* t-red (term t))])
      (if (empty? tvs)
          (if (tv? (term t))
              (term t)
              (error "no reductions found for type" (term t)))
          (first tvs)))])

(define tv? (redex-match doof-tc tv))

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
  
  [(kinds • t_a *)
   (kinds • t_r *)
   (where t_a2 (t-reduce t_a))
   (where t_r2 (t-reduce t_r))
   (types (x : t_a2 Γ) e t_b)
   (<: t_b t_r2)
   ------------------------------------------ "t-abs"
   (types Γ (λ (x t_a) t_r e) (-> t_a2 t_r2))]
  
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
   ------------------------------------------ "t-obj"
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
