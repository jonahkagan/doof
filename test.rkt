#lang racket

(require 
 rackunit
 rackunit/text-ui
 "ast.rkt"
 "parse.rkt"
 "interp.rkt"
 "type-check.rkt")

(define (check-parse sexpr expr)
  (check-equal? (parse sexpr) expr))

(define (check-tc-exn sexpr)
  (check-exn exn:fail? (lambda () (tc (parse sexpr)))))

(define (check-tc sexpr type)
  (check-equal? (tc (parse sexpr)) type))

(define (check-interp sexpr val)
  (define e (parse sexpr))
  (check-not-exn (lambda () (tc e)))
  (check-equal? (interp e) val))

(define (check-interp-exn sexpr)
  (define e (parse sexpr))
  (check-not-exn (lambda () (tc e)))
  (check-exn exn:fail? (lambda () (interp e))))

; Parse tests

(define all (t-str (pat-all)))
(define s (t-str (pat-str "s")))

(check-parse '"doofus" (s-str "doofus"))
(check-parse 'doofid (s-id 'doofid))

(check-parse '(apply this) 
             (s-app (s-id 'apply) (s-id 'this)))
(check-parse '((cat "that") "there")
             (s-app
              (s-app (s-id 'cat) (s-str "that"))
              (s-str "there")))

(check-parse '(lambda (x "s") -> "s" x)
             (s-lam (t-arrow s s) 'x (s-id 'x)))
(check-parse '(lambda (y String) -> String "why")
             (s-lam (t-arrow all all) 'y (s-str "why")))
(check-parse '(lambda (x ("s" -> String)) -> ("s" -> String) x)
             (s-lam (t-arrow (t-arrow s all)
                             (t-arrow s all))
                    'x (s-id 'x)))

(check-parse '((lambda (x String) -> String x) "ecks")
             (s-app (s-lam (t-arrow all all) 'x (s-id 'x)) (s-str "ecks")))

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

(check-parse '(lambda (o (Obj ("f" : String) ("g" : String))) -> String o)
             (s-lam (t-arrow
                     (t-obj (list (t-field (pat-str "f") all)
                                  (t-field (pat-str "g") all)))
                     all)
                    'o (s-id 'o)))


(check-parse '(if-empty (obj) "then" "else")
             (s-if-empty (s-obj) (s-str "then") (s-str "else")))

#;(check-parse '((def-rec (f x) x) (f "x"))
               (s-rec 'f (s-lam 'x (s-id 'x))
                      (s-app (s-id 'f) (s-str "x"))))

; Type checking tests

(check-tc-exn 'unbound)

(check-tc-exn '("doof" "us"))
(check-tc-exn '((cat "doof") (lambda (x String) -> String x)))
(check-tc-exn '(lambda (x String) -> (String -> String) x))

(check-tc-exn '((lambda (x String) -> String x)
                (lambda (x String) -> String x)))

(check-tc-exn '((lambda (x "doof") -> "doof" x) "us"))
(check-tc-exn '((lambda (f (String -> String)) -> String (f "s"))
                (lambda (x "doof") -> String x)))

(check-tc-exn '(get (obj) "doofus")) ; field not found
(check-tc-exn '(get (ext (obj) ("doofus" : "dodo")) "dodo"))

(check-tc-exn '(lambda (o (Obj ("doofus" : "dodo"))) -> String
                 (get o "dodo")))
(check-tc-exn '((lambda (o (Obj ("doofus" : "dodo"))) -> String
                  (get o "doofus"))
                (ext (obj) ("dodo" : "doofus"))))
(check-tc '((lambda (o (Obj ("doofus" : "dodo"))) -> String
                  (get o "doofus"))
            (ext (ext (obj) ("dodo" : "doofus")) ("doofus" : "dodo")))
          all)

#;(check-tc '((cat "doof") "us")
          (t-str (pat-str "doofus")))

#;(check-tc '((lambda (x String) -> String x) "doof")
            (t-str (pat-str "doof")))

; Interp tests
; (these also serve as type checking tests)

(define doofus (v-str "doofus"))
(define dodo (v-str "dodo"))

(check-interp '"doofus" doofus)

(check-interp '((lambda (x String) -> String x) "doofus") doofus)
(check-interp '((cat "doof") "us") doofus)
(check-interp '(((lambda (x String) -> (String -> String)
                   (lambda (y String) -> String
                     ((cat x) y)))
                 "do") "do")
              dodo)
(check-interp '(((lambda (x String) -> (String -> String)
                   (lambda (x String) -> String x))
                 "shadowed") "dodo")
              dodo)
(check-interp '((lambda (catter (String -> (String -> String))) -> String
                  ((catter "doof") "us"))
                cat)
              doofus)

(define o1 (v-obj (list (field "doofus" dodo))))
(define o2 (v-obj (list (field "dodo" doofus) (field "doofus" dodo))))

(check-interp '(obj) (v-obj empty))
(check-interp '(ext (obj) ("doofus" : "dodo"))
              o1)
(check-interp '(ext (ext (obj)
                         ("doofus" : "dodo")) 
                    ("dodo" : "doofus"))
              o2)
(check-interp '(ext (ext (obj) ("doofus" : "dodo")) ("doofus" : "doofus"))
              (v-obj (list (field "doofus" doofus))))

(check-interp '(ext (ext (obj) ("doofus" : "dodo")) ("dodo" : "doofus"))
              o2)
(check-interp '(ext (obj) (((cat "doof") "us")
                           : ((lambda (x String) -> String x) "dodo")))
              o1)

(check-interp '(get (ext (obj) ("doofus" : "dodo")) "doofus" )
              dodo)
#;(check-interp '(get (ext (ext (obj) ("dodo" : "doofus"))
                         ("doofus" : "dodo"))
                    ((cat "doof") "us"))
              dodo)
#;(check-interp '(get (ext (obj) ("doofus" : "dodo"))
                    ((lambda (x String) -> String x) "doofus"))
              dodo)

#;(check-interp '(names (obj))
                (v-obj empty))
#;(check-interp '(names (ext (ext (obj) ("doofus" : "dodo"))
                             ("dodo" : "doofus")))
                (v-obj (list (field "first" dodo)
                             (field "rest"
                                    (v-obj (list (field "first" doofus)
                                                 (field "rest" (v-obj empty))))))))

#;(check-interp '(if-empty (obj) "then" "else")
                (v-str "then"))
#;(check-interp '(if-empty (ext (obj) ("a" : "b")) "then" "else")
                (v-str "else"))

#;(check-interp '((def-rec (f x) x) (f "doofus"))
                doofus)
#;(check-interp '((def-rec (cat-reduce strs)
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

#;(check-interp '((def-rec (foldr f)
                    (lambda (list-obj)
                      (lambda (acc)
                        (if-empty
                         list-obj
                         acc
                         ((f (get list-obj "first"))
                          (((foldr f) (get list-obj "rest")) acc))))))
                  (((foldr (lambda (x String) -> String
                             (lambda (acc String) -> String
                               ((cat x) acc))))
                    (names
                     (ext (ext (ext (obj)
                                    ("a" : "A"))
                               ("b" : "B"))
                          ("c" : "C"))))
                   ""))
                (v-str "cba"))

; Simple dep obj-to-obj function that adds "my" to each field name
#;(check-interp '((def-rec (foldr f)
                    (lambda (list-obj Object) -> Object ; TODO change to obj
                      (lambda (acc String) -> String
                        (if-empty
                         list-obj
                         acc
                         ((f (get list-obj "first"))
                          (((foldr f) (get list-obj "rest")) acc))))))
                  ((lambda (orig String) -> String
                     (((foldr
                        (lambda (x)
                          (lambda (acc)
                            (ext acc (((cat "my") x) : (get orig x))))))
                       (names orig))
                      (obj)))
                   (ext (ext (ext (obj)
                                  ("a" : "A"))
                             ("b" : "B"))
                        ("c" : "C"))))
                
                (v-obj (list (field "myc" (v-str "C"))
                             (field "myb" (v-str "B"))
                             (field "mya" (v-str "A")))))