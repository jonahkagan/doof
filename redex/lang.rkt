#lang racket
(require redex)
(provide (all-defined-out))

(require "pat.rkt")

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
     (t-obj (p t) ...)
     X
     (tλ (X k) t)
     (t t))
  (X variable-not-otherwise-mentioned)
  ; Kinds
  (k *
     (=> k k)))

(define-metafunction doof
  distinct : any ... -> boolean
  [(distinct any_1 ...)
   ,(let ([as (term (any_1 ...))]) 
      (= (length as)
         (length (remove-duplicates as))))])
