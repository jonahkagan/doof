#lang racket
(require redex)
(provide (all-defined-out))

; String patterns
(define-language pat
  (p str ; the set of all strings
     string
     (p-cat p p)))

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

; might want to make this a reduction relation...
(define-metafunction pat
  pat-reduce : p -> p
  [(pat-reduce (p-cat string_1 string_2))
   ,(string-append (term string_1) (term string_2))]
  [(pat-reduce (p-cat p_1 p_2))
   (p-cat (pat-reduce p_1) p_2)
   (side-condition (not (string? (term p_1))))]
  [(pat-reduce (p-cat string_1 p_2))
   (p-cat string_1 (pat-reduce p_2))
   (side-condition (not (string? (term p_2))))]
  [(pat-reduce p) p])

(define (pat-reducible? p)
  (not (or (redex-match? pat str p)
           (redex-match? pat string p))))