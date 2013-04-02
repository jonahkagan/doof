#lang racket
(require redex)
(provide (all-defined-out))

; String patterns
(define-language pat
  (p str ; the set of all strings
     string
     (p-cat p p))
  (pv str
      string
      (p-cat pv pv))
  (pE hole
      (p-cat pE pv)
      (p-cat pv pE)))

; Sub-pattern relation
(define-relation pat
  <p ⊆ p × p
  [(<p p p)]
  [(<p string str)]
  [(<p (p-cat p p) str)]
  [(<p (p-cat p_1 p_2) p_3)
   (where p_3 (pat-reduce (p-cat p_1 p_2)))]
  [(<p p_1 (p-cat p_2 p_3))
   (where p_1 (pat-reduce (p-cat p_2 p_3)))])

(define p-red
  (reduction-relation
   pat
   #:domain p
   
   (==> (p-cat string_1 string_2)
        ,(string-append (term string_1) (term string_2)))
   (==> (p-cat p "") p)
   (==> (p-cat "" p) p)
   (==> (p-cat str str) str)
   
   with
   [(--> (in-hole pE p_1) (in-hole pE p_2))
    (==> p_1 p_2)]))

(define-metafunction pat
  pat-reduce : p -> p
  [(pat-reduce p)
   ,(first (apply-reduction-relation* p-red (term p)))])