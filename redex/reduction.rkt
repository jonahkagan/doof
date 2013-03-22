#lang racket
(require redex)
(provide (all-defined-out))

(require "lang.rkt"
         "typing.rkt")

(define-extended-language doof-ctx doof-tc
  ; Values
  (v string
     (obj (string v) ...)
     (λ (x t) t e))
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
   #:domain e
   (==> ((λ (x t) t e) v) (subst x v e)
        "e-app")
   (==> (cat string_1 string_2) (str-cat string_1 string_2)
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

(define-metafunction doof-ctx
  subst : x v e -> e
  [(subst x v x) v]
  [(subst x v y) y]
  [(subst x v (λ (x t_a) t_r e)) (λ (x t_a) t_r e)]
  [(subst x v (λ (y t_a) t_r e)) (λ (y t_a) t_r (subst x v e))]
  ; boring cases
  [(subst x v string) string]
  [(subst x v (e_1 e_2)) ((subst x v e_1) (subst x v e_2))]
  [(subst x v (cat e_1 e_2)) (cat (subst x v e_1) (subst x v e_2))]
  [(subst x v (obj (string e) ...)) (obj (string (subst x v e)) ...)]
  [(subst x v (get e_1 e_2)) (get (subst x v e_1) (subst x v e_2))]
  [(subst x v (ext e_1 e_2 e_3))
          (ext (subst x v e_1) (subst x v e_2) (subst x v e_3))])

