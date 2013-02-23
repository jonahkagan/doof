#lang typed/racket

(provide
 (rename-out [tc-expr tc]))

(require "ast.rkt")

(define str (t-str (pat-all)))

(define: (subtype? [t1 : Type] [t2 : Type]) : Boolean
  (match (cons t1 t2)
    [(cons (t-str s1) (t-str s2))
     (subpat? s1 s2)]
    [(cons (t-arrow arg1 ret1) (t-arrow arg2 ret2))
     (and (subtype? arg2 arg1) (subtype? ret1 ret2))]
    [_ (error "can't check subtype" t1 t2)]))

(define: (subpat? [p1 : Pat] [p2 : Pat]) : Boolean
  (match (cons p1 p2)
    [(cons (pat-str p1) (pat-str p2)) (equal? p1 p2)]
    [(cons _ (pat-all)) true]
    [_ false]))
          
(define: (tc-expr [e : Expr]) : Type
  (tc e mt-env))

(define: (tc [e : Expr] [env : TyEnv]) : Type
  (match e
    [(s-str s) (t-str (pat-str s))]
    
    [(s-id id)
     (match (lookup id env)
       [(None) (error (string-append "Unbound id: " (symbol->string id)))]
       [(Some t) t])]
    
    [(s-prim name arg)
     (match (tc-prim name)
       [(t-arrow argt rett)
        (cond
          [(subtype? (tc arg env) argt) rett]
          [else (error "prim arg is wrong type" name)])])]
    
    [(s-lam type arg body)
     (match type
       [(t-arrow argt rett)
        (cond
          [(subtype? (tc body (extend-env (bind arg argt) env))
                     rett)
           (t-arrow argt rett)]
          [else (error "lambda type mismatch")])])]
    
    ;[(s-rec name lam rest) ...]
    
    [(s-app fun arg)
     (match (tc fun env)
       [(t-arrow argt rett)
        (cond
          [(subtype? (tc arg env) argt) rett]
          [else (error "function type did not match arg type")])]
       [_ (error "can't apply non-function")])]
    
    ;[(s-obj) ...]
    
    ;[(s-get obj field) ...]
    
    ;[(s-ext obj field val) ...]
    
    ;[(s-if-empty obj then else) ...]
    ))

(define (tc-prim name)
  (case name
    [(cat) (t-arrow str (t-arrow str str))]
    [(cat2) (t-arrow str str)]
    ;[(names) ...]
    [else (error "unknown prim" name)]))