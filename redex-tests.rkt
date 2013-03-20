#lang racket
(require redex
         "redex.rkt")

(define (progress-holds? e)
  (if (types? e)
      (or (v? e)
          (reduces? e))
      #t))

(define (preservation-holds? e)
  (let ([ts (types-of e)])
    (if (not (null? ts))
        (let ([vs (reds-of e)])
          (if (not (null? vs))
              (andmap
               (λ (v)
                 (equal?
                  ts
                  (types-of v)))
               vs)
              #t))
        #t)))

(define (test-pred pred)
  (let ([c (make-coverage red)])
    (parameterize ([relation-coverage (list c)])
      (check-reduction-relation red pred)
      (covered-cases c))))

(define (types-of e)
  (judgment-holds (types · ,e t) t))

(define (reds-of e)
  (apply-reduction-relation red e))

(define (types? e)
  (not (null? (types-of e))))

(define v? (redex-match Ev v))

(define (reduces? e)
  (not (null? (reds-of e))))

; Typing

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

; Evaluation

(test-->>
 red
 (term (cat "a" "b"))
 (term "ab"))

(test-->>
 red
 (term (cat "a" ((λ (x str) x) "b")))
 (term "ab"))

(test-->>
 red
 (term (((λ (f (-> str str))
           (λ (x str) (f x)))
         (λ (x str) (cat "a" x)))
        "b"))
 (term "ab"))

(test-->>
 red
 (term (((λ (x str)
           (λ (x str) x))
         "a") "b"))
 "b")

(test-results)