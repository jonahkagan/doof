#lang racket
(require redex)
(provide (all-defined-out))

(require "pat.rkt")

(define-extended-language doof pat
  ; Expressions
  (e string
     boolean
     x
     (λ (x t) t e)
     (λ (x t) e)
     (e e)
     (cat e e)
     (obj (string e) ...)
     (ext e e e)
     (get e e)
     (fold e e e)
     (if e e e))
  ((x y) variable-not-otherwise-mentioned)
  ; Types
  (t p
     bool
     Top
     (-> t t)
     (t-cat t t)
     (t-obj (string t) ...)
     (t-ext t t t)
     (t-fold t t t)
     X
     (tλ (X k) t)
     (t t))
  ((X Y) variable-not-otherwise-mentioned)
  ; Kinds
  (k *
     (=> k k)))

(define-metafunction doof
  distinct : any ... -> boolean
  [(distinct any_1 ...)
   ,(let ([as (term (any_1 ...))]) 
      (= (length as)
         (length (remove-duplicates as))))])
