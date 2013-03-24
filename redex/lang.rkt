#lang racket
(require redex)
(provide (all-defined-out))

(require "pat.rkt")

(define-extended-language doof pat
  ; Expressions
  (e string
     x
     (λ (x t) t e)
     (e e)
     (cat e e)
     (obj (string e) ...)
     (ext e e e)
     (get e e))
  ((x y) variable-not-otherwise-mentioned)
  ; Types
  (t p
     (-> t t)
     (t-cat t t)
     (t-obj (string t) ...)
     (t-ext t t t)
     X
     (tλ (X k) t)
     (t t))
  ((X Y) variable-not-otherwise-mentioned)
  ; Kinds
  (k *
     (=> k k)))

(define-metafunction doof
  str-cat : string string -> string
  [(str-cat string_1 string_2)
   ,(string-append (term string_1) (term string_2))])

(define-metafunction doof
  distinct : any ... -> boolean
  [(distinct any_1 ...)
   ,(let ([as (term (any_1 ...))]) 
      (= (length as)
         (length (remove-duplicates as))))])
