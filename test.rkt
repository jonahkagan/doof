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
  ;(check-not-exn (lambda () (tc e)))
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

(check-parse '(fold (lambda (x String) -> String x)
                    "" o)
             (s-fold (s-lam (t-arrow all all) 'x (s-id 'x))
                     (s-str "")
                     (s-id 'o)))

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
(check-tc-exn '((lambda (o (Obj ("doofus" : "dodo"))) -> (Obj) o)
                (ext (obj) ("doofus" : (lambda (x String) -> String x)))))

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

(check-interp 
 '(fold (lambda (name String) -> (String -> (String -> String))
          (lambda (value String) -> (String -> String)
            (lambda (acc String) -> String
              ((cat name) acc))))
        "" 
        (ext (ext (ext (obj) ("a" : "1")) ("b" : "2")) ("c" : "3")))
 (v-str "cba"))

; Simple dep obj-to-obj program that adds "my" to each field name
(check-interp
 '(fold (lambda (name String) -> (String -> ((Obj) -> (Obj)))
          (lambda (value String) -> ((Obj) -> (Obj))
            (lambda (acc (Obj)) -> (Obj)
              (ext acc (((cat "my") name) : value)))))
        (obj)
        (ext (ext (ext (obj) ("a" : "1")) ("b" : "2")) ("c" : "3")))
 (v-obj (list (field "myc" (v-str "3"))
              (field "myb" (v-str "2"))
              (field "mya" (v-str "1")))))