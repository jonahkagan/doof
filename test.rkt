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
             (s-obj))

(check-parse '(get (obj) "f")
             (s-get (s-obj) (s-str "f")))
(check-parse '(get o f)
             (s-get (s-id 'o) (s-id 'f)))

(check-parse '(ext (obj) ("f" : "v"))
             (s-ext (s-obj) (s-str "f") (s-str "v")))
(check-parse '(ext o (f : v))
             (s-ext (s-id 'o) (s-id 'f) (s-id 'v)))
(check-parse '(ext (ext (obj) ("first" : Doof)) (last : "Us"))
             (s-ext (s-ext (s-obj) (s-str "first") (s-id 'Doof))
                    (s-id 'last) (s-str "Us")))

(check-parse '(if-empty (obj) "then" "else")
             (s-if-empty (s-obj) (s-str "then") (s-str "else")))

(check-parse '((def-rec (f x) x) (f "x"))
             (s-rec 'f (s-lam 'x (s-id 'x))
                    (s-app (s-id 'f) (s-str "x"))))

;; Interp tests

(define doofus (v-str "doofus"))
(define dodo (v-str "dodo"))

(check-interp '"doofus" doofus)
(check-interp-exn 'doofid) ; unbound id

(check-interp '((lambda (x) x) "doofus") doofus)
(check-interp '((cat "doof") "us") doofus)
(check-interp '(((lambda (x)
                   (lambda (y)
                     ((cat x) y)))
                 "do") "do")
              dodo)
(check-interp '(((lambda (x)
                   (lambda (x) x))
                 "shadowed") "dodo")
              dodo)

(define o1 (v-obj (list (field "doofus" dodo))))
(define o2 (v-obj (list (field "dodo" doofus) (field "doofus" dodo))))

(check-interp '(obj) (v-obj empty))
(check-interp '(ext (obj) ("doofus" : "dodo"))
              o1)
(check-interp '(ext (ext (obj) ("doofus" : "dodo")) ("dodo" : "doofus"))
              o2)
(check-interp '(ext (ext (obj) ("doofus" : "dodo")) ("doofus" : "doofus"))
              (v-obj (list (field "doofus" doofus))))

(check-interp '(ext (ext (obj) ("doofus" : "dodo")) ("dodo" : "doofus"))
              o2)
(check-interp '(ext (obj) (((cat "doof") "us") : ((lambda (x) x) "dodo")))
              o1)

(check-interp '(get (ext (obj) ("doofus" : "dodo")) "doofus" )
              dodo)
(check-interp-exn '(get (obj)  "doofus")) ; field not found
(check-interp '(get (ext (ext (obj) ("dodo" : "doofus"))
                         ("doofus" : "dodo"))
                    ((cat "doof") "us"))
              dodo)

(check-interp '(names (obj))
              (v-obj empty))
(check-interp '(names (ext (ext (obj) ("doofus" : "dodo"))
                           ("dodo" : "doofus")))
              (v-obj (list (field "first" dodo)
                          (field "rest"
                                (v-obj (list (field "first" doofus)
                                            (field "rest" (v-obj empty))))))))

(check-interp '(if-empty (obj) "then" "else")
              (v-str "then"))
(check-interp '(if-empty (ext (obj) ("a" : "b")) "then" "else")
              (v-str "else"))

(check-interp '((def-rec (f x) x) (f "doofus"))
              doofus)
(check-interp '((def-rec (cat-reduce strs)
                  (if-empty strs
                            ""
                            ((cat (get strs "first"))
                             (cat-reduce (get strs "rest")))))
                (cat-reduce 
                 (ext (ext (obj)
                           ("first" : "doof"))
                      ("rest" : (ext (ext (obj)
                                          ("first" : "us"))
                                     ("rest" : (obj)))))))
              doofus)

(check-interp '((def-rec (foldr f)
                  (lambda (list-obj)
                    (lambda (acc)
                      (if-empty
                       list-obj
                       acc
                       ((f (get list-obj "first"))
                        (((foldr f) (get list-obj "rest")) acc))))))
                (((foldr (lambda (x)
                           (lambda (acc)
                             ((cat x) acc))))
                  (names
                   (ext
                    (ext
                     (ext
                      (obj)
                      ("a" : "A"))
                     ("b" : "B"))
                    ("c" : "C"))))
                 ""))
              (v-str "cba"))


