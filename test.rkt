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

;; Parse tests

(check-parse '"doofus" (s-str "doofus"))
(check-parse 'doofid (s-id 'doofid))

(check-parse '(apply this) 
             (s-app (s-id 'apply) (s-id 'this)))
(check-parse '((cat "that") "there")
             (s-app
              (s-prim 'cat (s-str "that"))
              (s-str "there")))

(check-parse '(lambda (x) x)
             (s-lam 'x (s-id 'x)))
(check-parse '(lambda (y) "why")
             (s-lam 'y (s-str "why")))

(check-parse '((lambda (x) x) "ecks")
             (s-app (s-lam 'x (s-id 'x)) (s-str "ecks")))

(check-parse '(obj)
             (s-obj '()))
(check-parse '(obj ("first" : Doof) (last : "Us"))
             (s-obj (list (cons (s-str "first") (s-id 'Doof))
                          (cons (s-id 'last) (s-str "Us")))))
(check-parse '(obj (((cat "do") "of") : (f "us")))
             (s-obj (list (cons (s-app (s-prim 'cat (s-str "do"))
                                       (s-str "of"))
                                (s-app (s-id 'f) (s-str "us"))))))

(check-parse '(get (obj) "f")
             (s-get (s-obj '()) (s-str "f")))
(check-parse '(get o f)
             (s-get (s-id 'o) (s-id 'f)))

(check-parse '(ext (obj) ("f" : "v"))
             (s-ext (s-obj '()) (s-str "f") (s-str "v")))
(check-parse '(ext o (f : v))
             (s-ext (s-id 'o) (s-id 'f) (s-id 'v)))


;; Interp tests

(define doofus (v-str "doofus"))
(define heyho (v-str "heyho"))

(check-interp '"doofus" doofus)
(check-interp-exn 'doofid) ; unbound id

(check-interp '((cat "doof") "us") doofus)
(check-interp '((lambda (x) x) "doofus") doofus)
(check-interp '(((lambda (x)
                   (lambda (y)
                     ((cat x) y)))
                 "hey") "ho")
              heyho)
(check-interp '(((lambda (x)
                   (lambda (x) x))
                 "let's go") "heyho")
              heyho)

(define o1 (v-obj (make-immutable-hash (list (cons "doofus" heyho)))))
(define o2 (v-obj (make-immutable-hash 
                   (list (cons "doofus" heyho) (cons "heyho" doofus)))))

(check-interp '(obj) (v-obj (hash)))
(check-interp '(obj ("doofus" : "heyho") ("heyho" : "doofus"))
              o2)
(check-interp '(obj (((cat "doof") "us") : ((lambda (x) x) "heyho")))
              o1)

(check-interp '(get (obj ("doofus" : "heyho")) "doofus" )
              heyho)
(check-interp-exn '(get (obj)  "doofus")) ; field not found
(check-interp '(get (obj ("heyho" : "doofus")
                         ("doofus" : "heyho"))
                    ((cat "doof") "us"))
              heyho)

(check-interp '(ext (obj) ("doofus" : "heyho"))
              o1)
(check-interp '(ext (obj ("doofus" : "heyho")) ("heyho" : "doofus"))
              o2)
(check-interp '(ext (obj ("doofus" : "heyho")) ("doofus" : "doofus"))
              (v-obj (make-immutable-hash (list (cons "doofus" doofus)))))