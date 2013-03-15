#lang typed/racket

(provide
 (rename-out [interp-expr interp]))

(require racket/match)
(require "ast.rkt")

;(define exposed-prims '(cat))

(define extend-env #{poly-extend-env @ Value})

(: err (String Any * -> Nothing))
(define (err msg . vals)
  (apply error 'interp msg vals))

; Interpreter
(define: (interp-expr [e : Expr]) : Value
  (interp e mt-env))

(define: (interp [e : Expr] [env : Env]) : Value
  (match e
    [(s-str s) (v-str s)]
    
    [(s-id id)
     (match (lookup id env)
       [(None) (err "Unbound id: ~a" id)]
       [(Some v) v])]
    
    [(s-lam _ arg body)
     (v-clos arg body env)]
    
    [(s-app fun arg)
     (app (interp fun env) (interp arg env) env)]
    
    [(s-ty-lam _ body) (interp body env)]
    [(s-ty-app fun _) (interp fun env)]
    
    [(s-cat e1 e2)
     (match (interp e1 env)
       [(v-str s1)
        (match (interp e2 env)
          [(v-str s2) (v-str (string-append s1 s2))]
          [_ (err "cat: expected string as second arg")])]
       [_ (err "cat: expected string as first arg")])]
    
    [(s-obj)
     (v-obj empty)]
    
    [(s-get obj field)
     (match (interp obj env)
       [(v-obj fields)
        (match (interp field env)
          [(v-str name)
           (match (fields-get fields name)
             [(Some v) v]
             [(None) (err "get: field not found")])]
          [_ (err "get: field name must be a string")])]
       [_ (err "get: can't get from a non-object")])]
    
    [(s-ext obj field val)
     (match (interp obj env)
       [(v-obj fields)
        (match (interp field env)
          [(v-str name)
           (v-obj (fields-ext fields name (interp val env)))]
          [_ (err "ext: field name must be a string")])]
       [_ (err "ext: expected object")])]
    
    [(s-fold fun acc obj)
     (define funv (interp fun env))
     (match (interp obj env)
       [(v-obj fields)
        (foldr (lambda: ([f : field] [accv : Value])
                 (app (app (app funv (v-str (field-name f)) env)
                           (field-value f) env)
                      accv env))
               (interp acc env)
               fields)]
       [_ (err "fold: expected object")])]
    ))

(define: (app [clos : Value] [arg : Value] [env : Env]) : Value
  (match clos
    [(v-clos carg cbody cenv)
     (interp cbody
             (extend-env (bind carg arg) cenv))]
    [else (err "can't apply non-function value")]))

; Object field helpers
(define: (fields-get [fields : (Listof field)] [name : String])
  : (Opt Value)
  (cond
    [(empty? fields) (None)]
    [(equal? name (field-name (first fields)))
     (Some (field-value (first fields)))]
    [else (fields-get (rest fields) name)]))

(define: (fields-ext [fields : (Listof field)] [name : String]
                     [val : Value]) : (Listof field)
  (cons (field name val)
        (filter (lambda: ([f : field])
                  (not (equal? (field-name f) name)))
                fields)))

(define: (value->string [v : Value]) : String
  (match v
    [(v-str s) s]
    [(v-clos arg body env)
     (format "(lambda (~a) ...)" arg)]
    [(v-obj fields) "(obj ...)"]))
