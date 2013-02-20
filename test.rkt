#lang racket

(require 
 rackunit
 rackunit/text-ui
 "doof.rkt")

(define (check-parse sexpr expr)
  (check-equal? (parse sexpr) expr))

(define (check-interp sexpr val)
  (check-equal? (interp (parse sexpr)) val))

(define (check-interp-exn sexpr)
  (check-exn exn:fail? (lambda () (interp (parse sexpr)))))

(define parse-tests
  (test-suite
   "parse"
   
   (check-parse '"doofus" (s-str "doofus"))
   (check-parse 'doofid (s-id 'doofid))
   
   (check-parse '(apply this) (s-app (s-id 'apply) (s-id 'this)))
   (check-parse '((cat "that") "there") (s-app
                                         (s-prim 'cat (s-str "that"))
                                         (s-str "there")))
   
   (check-parse '(lambda (x) x) (s-lam 'x (s-id 'x)))
   (check-parse '(lambda (y) "why") (s-lam 'y (s-str "why")))
   
   (check-parse '((lambda (x) x) "ecks")
                (s-app (s-lam 'x (s-id 'x)) (s-str "ecks")))
   ))

(define interp-tests
  (test-suite
   "interp"
   
   (check-interp '"doofus" "doofus")
   (check-interp-exn 'doofid) ; unbound id
   
   (check-interp '((cat "lol") "ol") "lolol")
   (check-interp '((lambda (x) x) "ecks") "ecks")
   (check-interp '(((lambda (x)
                    (lambda (y)
                      ((cat x) y)))
                  "hey") "ho")
               "heyho")
   (check-interp '(((lambda (x)
                    (lambda (x) x))
                  "shade") "shadow")
               "shadow")
   ))

(run-tests parse-tests)
(run-tests interp-tests)