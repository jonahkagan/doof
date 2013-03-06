#lang typed/racket

(provide
 (rename-out [interp-expr interp]))

(require racket/match)
(require "ast.rkt")

(define exposed-prims '(cat))

(define extend-env #{poly-extend-env @ Value})

(: err (String Any * -> Nothing))
(define (err msg . vals)
  (apply error 'interp msg vals))

; Interpreter
(define: (interp-expr [e : Expr]) : Value
  (interp e initial-env))

(define: initial-env : Env
  (foldl (lambda: ([name : Symbol] [env : Env])
           (extend-env (bind name (v-prim name)) env))
         mt-env
         exposed-prims))

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
    [(v-prim name) (app-prim name arg env)]
    [else (err "can't apply non-function value")]))

(define: (app-prim [name : Symbol] [arg : Value] [env : Env]): Value
  (case name
    ; Bit of a trick to make cat a one arg lambda that will be curried
    ; with the first string. We create a closure and bind the first string
    ; in its env as s1. Then we call cat2, which knows to look up s1 in the
    ; env which is passed into the call to prim.
    [(cat)
     (v-clos 's2
             (s-app (s-id 'cat2) (s-id 's2))
             (extend-env (bind 's1 arg) 
                         (extend-env (bind 'cat2 (v-prim 'cat2))
                                     mt-env)))]
    [(cat2)
     (match (lookup 's1 env)
       [(Some (v-str s1))
        (match arg
          [(v-str s2)
           (v-str (string-append s1 s2))]
          [_ (err "cat: expected string as second arg")])]
       [_ (err "cat: expected string as first arg")])]
    [else (err "unknown prim: ~a" name)]))

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
