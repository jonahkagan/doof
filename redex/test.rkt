#lang racket
(require redex)

(require "lang.rkt"
         "kinding.rkt"
         "typing.rkt"
         "reduction.rkt")

(define (progress-holds? e)
  (if (types? e)
      (or (v? e)
          (reduces? e))
      #t))

(define (preservation-holds? e)
  (let ([ts (types-of e)])
    (cond
      [(null? ts) #t]
      [(> (length ts) 1) #f]
      [else
       (let ([vs (reds-of e)])
         (cond
           [(null? vs) #t]
           [(> (length vs) 1) #f]
           [else
            (let ([tvs (types-of (first vs))])
              (cond
                [(null? tvs) #f]
                [(> (length tvs) 1) #f]
                [else
                 (term (<: ,(first tvs) ,(first ts)))]))]))])))

(define (check-pred pred)
  (let ([c (make-coverage red)])
    (parameterize ([relation-coverage (list c)])
      (check-reduction-relation red pred)
      (covered-cases c))))

(define (types-of e)
  (judgment-holds (types • ,e t) t))

(define (reds-of e)
  (apply-reduction-relation* red e))

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
      (judgment-holds (types • e t) t)
      (list (term expected)))]
    [(test-types Γ e expected)
     (test-equal
      (judgment-holds (types Γ e t) t)
      (list (term expected)))]))

(test-types
 (x : str (x : (-> str str) •))
 x
 str)

(test-types
 (y : str (x : (-> str str) •))
 x
 (-> str str))

(test-types ((λ (x "a") "a" x) "a")
            "a")

(test-types ((λ (x str) str x) "a")
            str)

(test-types ((λ (x str) x) "a")
            str)

(test-types #t bool)

(test-equal (types-of (term ((λ (a str) "b" a) "b")))
            empty)

(test-types (obj ("f" "x"))
            (t-obj ("f" "x")))

(test-types (ext (obj) "f" "x")
            (t-obj ("f" "x")))

(test-types (ext (ext (obj) "f" "x") "g" "y")
            (t-obj ("g" "y") ("f" "x")))

(test-types (ext (ext (obj) "f" "x") "f" "y")
            (t-obj ("f" "y")))

(test-types ((λ (o (t-obj ("f" str))) (t-obj) o) 
             (obj ("f" "1") ("g" "2")))
            (t-obj))

(test-types ((λ (o (t-obj ("f" "1"))) (t-obj ("f" "1") ("g" str))
               (ext o "g" "2"))
             (obj ("f" "1")))
            (t-obj ("f" "1") ("g" str)))

(test-types ((λ (s "a") (t-cat "a" "b")
               (cat s "b"))
             "a")
            "ab")

(test-types ((λ (o (t-obj)) (t-ext (t-obj) "f" str)
               (ext o "f" "1"))
             (obj ("g" "2")))
            (t-obj ("f" str)))

(test-types (fold (λ (name str) (-> str (-> str str))
                    (λ (val str) (-> str str)
                      (λ (acc str) str
                        (cat name acc))))
                  ""
                  (obj ("a" "1") ("b" "2") ("c" "3")))
            "abc")

(test-types (if #t "then" "else")
            str)

(test-types (if #t (λ (s str) s) (λ (s "a") s))
            (-> "a" str))


; Type operators

(define-syntax-rule (test-t-red t expected)
  (test-->> t-red (term t) (term expected)))

(test-t-red ((tλ (X *) X) ((tλ (Y *) Y) "a"))
            "a")

(test-t-red ((tλ (X *) (-> X X)) str)
            (-> str str))

(test-types ((λ (a str) ((tλ (X *) X) str) a) "b")
            str)

(test-types ((λ (o (t-obj ("f" str)))
               (t-fold (tλ (N *)
                           (tλ (V *)
                               (tλ (A *)
                                   (t-ext A N V))))
                       (t-obj)
                       (t-obj ("f" str)))
               o)
             (obj ("f" "1")))
            (t-obj ("f" str)))

(test-types ((λ (o (t-obj ("f" str)))
               ((tλ (O *)
                    (t-fold (tλ (N *)
                                (tλ (V *)
                                    (tλ (A *)
                                        (t-ext A N V))))
                            (t-obj)
                            O))
                (t-obj ("f" str)))
               (fold (λ (n str) (-> str (-> (t-obj) (t-obj)))
                       (λ (v str) (-> (t-obj) (t-obj))
                         (λ (a (t-obj)) (t-obj)
                           (ext a n v))))
                     (obj)
                     o))
             (obj ("f" "1")))
            (t-obj ("f" str)))

(test-types ((λ (o (t-obj ("f" str) ("g" str)))
               ((tλ (O *)
                    (t-fold (tλ (N *)
                                (tλ (V *)
                                    (tλ (A *)
                                        (t-ext A (t-cat "my" N) V))))
                            (t-obj)
                            O))
                (t-obj ("f" str) ("g" str)))
               ; "i" because these annotations will be ignored
               (fold (λ (n "i")
                       (λ (v "i")
                         (λ (a "i")
                           (ext a (cat "my" n) v))))
                     (obj)
                     o))
             (obj ("f" "1") ("g" "2")))
            (t-obj ("myf" str) ("myg" str)))

(test-types ((λ (o (t-obj ("a" str) ("b" str) ("c" str)))
               ((tλ (O *)
                    (t-fold (tλ (N *)
                                (tλ (V *)
                                    (tλ (A *)
                                        (t-cat V A))))
                            ""
                            O))
                (t-obj ("a" str) ("b" str) ("c" str)))
               (fold (λ (name str) (-> str (-> str str))
                       (λ (val str) (-> str str)
                         (λ (acc str) str
                           (cat val acc))))
                     ""
                     o))
             (obj ("a" "1") ("b" "2") ("c" "3")))
            str)

(types-of
 (term (λ (o (t-obj ("x" str) ("y" str)))
         (fold (λ (name "i") "i"
                 (λ (val "i") "i"
                   (λ (acc "i") "i"
                     (ext
                      (ext 
                       (ext acc
                            name
                            val)
                       (cat name "_w__")
                       #f)
                      (cat name "_v__")
                      val))))
               (obj)
               o))))

#| this is a kind error - we don't know that S is a string type
(test-types ((λ (s "a") ((tλ (S *) (t-cat S "b")) "a")
               (cat s "b"))
             "a")
            "ab")
|#             


; Evaluation

(define (build-obj fields)
  (foldl (λ (f o)
           (term (ext ,o ,(car f) ,(cdr f))))
         (term (obj))
         fields))

(define-syntax-rule (test-red e expected)
  (test-->> red (term e) (term expected)))

(test-red (cat "a" "b") "ab")

(test-red (cat "a" ((λ (x str) str x) "b"))
          "ab")

(test-red (((λ (f (-> str str)) (-> str str)
              (λ (x str) str (f x)))
            (λ (x str) str (cat "a" x)))
           "b")
          "ab")

(test-red (((λ (x str) (-> str str)
              (λ (x str) str x))
            "a") "b")
          "b")

(test-red (ext (obj) "f" "x")
          (obj ("f" "x")))

(test-red (ext (ext (obj) "f" "x") "f" "y")
          (obj ("f" "y")))

(test-red ,(build-obj '(("x" . "a")
                        ("f" . "b")
                        ("g" . "c")
                        ("f" . "d")
                        ("g" . "e")
                        ("y" . "h")))
          (obj ("y" "h")
               ("g" "e")
               ("f" "d")
               ("x" "a")))

(test-red (get (obj ("f" "x") ("g" "y")) (cat "g" ""))
          "y")

(test-red (fold (λ (name str) (-> str (-> str str))
                  (λ (val str) (-> str str)
                    (λ (acc str) str
                      (cat name acc))))
                ""
                (obj ("a" "1") ("b" "2") ("c" "3")))
          "abc")

(test-red (if (if #t #f #t)
              "a" "b")
          "b")
              

(test-results)
