#lang racket
(require redex)
(provide (all-defined-out))

(require "lang.rkt"
         "typing.rkt")

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
   #:domain e
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
