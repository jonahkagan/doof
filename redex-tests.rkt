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
                 (andmap
                  (λ (t1 t2)
                    (type-equiv? ,t1 ,t2))
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

(define-syntax-rule (type-equiv? t1 t2)
  (and (term (<: t1 t2))
       (term (<: t2 t1))))

(define (types-of e)
  (judgment-holds (types · ,e t) t))

(define (reds-of e)
  (apply-reduction-relation red e))

(define (types? e)
  (not (null? (types-of e))))

(define v? (redex-match doof-ctx v))

(define (reduces? e)
  (not (null? (reds-of e))))

; Typing

(define-syntax test-types
  (syntax-rules ()
    [(test-types e expected)
     (test-equal
      (judgment-holds (types · e t) t)
      (list (term expected)))]
    [(test-types Γ e expected)
     (test-equal
      (judgment-holds (types Γ e t) t)
      (list (term expected)))]))

(test-types
 (x : str (x : (-> str str) ·))
 x
 str)

(test-types
 (y : str (x : (-> str str) ·))
 x
 (-> str str))

(test-types ((λ (x "a") x) "a")
            "a")

(test-types ((λ (x str) x) "a")
            "a")


; Evaluation

(define-syntax-rule (test-red e expected)
  (test-->> red (term e) (term expected)))

(test-red (cat "a" "b") "ab")

(test-red (cat "a" ((λ (x str) x) "b"))
          "ab")

(test-red (((λ (f (-> str str))
              (λ (x str) (f x)))
            (λ (x str) (cat "a" x)))
           "b")
          "ab")

(test-red (((λ (x str)
              (λ (x str) x))
            "a") "b")
          "b")

(test-results)