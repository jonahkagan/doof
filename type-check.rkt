#lang typed/racket

(provide
 (rename-out [tc-expr tc]))

(require "ast.rkt")

(define str (t-str (pat-all)))

(define: (tc-expr [e : Expr]) : Type
  (tc e mt-env))

(define: (tc [e : Expr] [env : TyEnv]) : Type
  (match e
    ; Later we will use singleton string patterns, but for now, just
    ; give everything type String
    [(s-str s) str]
    
    [(s-id id)
     (match (lookup id env)
       [(None) (error (string-append "Unbound id: " (symbol->string id)))]
       [(Some t) t])]
    
    [(s-prim name arg)
     (match (tc-prim name)
       [(t-arrow argt rett)
        (cond
          [(equal? argt (tc arg env)) rett]
          [else (error "prim arg is wrong type" name)])])]
    
    [(s-lam type arg body)
     (match type
       [(t-arrow argt rett)
        (cond
          [(equal? (tc body (extend-env (bind arg argt) env))
                   rett)
           (t-arrow argt rett)]
          [else (error "lambda type mismatch")])])]
    
    ;[(s-rec name lam rest) ...]
    
    [(s-app fun arg)
     (match (tc fun env)
       [(t-arrow argt rett)
        (cond
          [(equal? argt (tc arg env)) rett]
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