#lang racket
(require redex
         "redex.rkt")

(test-equal
 (judgment-holds
  (types (x : str (x : (-> str str) ·))
         x
         t)
  t)
 (list (term str)))

(test-equal
 (judgment-holds
  (types (y : str (x : (-> str str) ·))
         x
         t)
  t)
 (list (term (-> str str))))

(test-results)