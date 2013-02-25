#lang typed/racket

(provide
 (rename-out [interp-expr interp]))

(require racket/match)
(require "ast.rkt")

(define exposed-prims '(cat names))

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
    
    [(s-rec name lam rest)
     (define: rec-binding : (binding Value) (bind name (v-str "dummy")))
     (define rec-env (extend-env rec-binding env))
     (define clos (interp lam rec-env))
     (match clos
       [(v-clos _ _ _)
        (set-binding-value! rec-binding clos)
        (interp rest rec-env)]
       [_ (err "rec: somehow got a non-function")])]
    
    [(s-app fun arg)
     (match (interp fun env)
       [(v-clos carg cbody cenv)
        (interp cbody
                (extend-env (bind carg (interp arg env)) cenv))]
       [(v-prim name) (apply-prim name (interp arg env) env)]
       [else (err "can't apply non-function value")])]
    
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
    
    [(s-if-empty obj then else)
     (match (interp obj env)
       [(v-obj fields) 
        (cond
          [(empty? fields)
           (interp then env)]
          [else (interp else env)])]
       [_ (err "if-empty: expected object")])]
    ))

(define: (apply-prim [name : Symbol] [arg : Value] [env : Env]): Value
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
    [(names)
     (match arg
       [(v-obj fields)
        (list->doof-list (map v-str (fields-names fields)))]
       [_ (err "names: expected object")])]
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

(define: (fields-names [fields : (Listof field)]) : (Listof String)
  (map field-name fields))

; Constructs an object representing a list that has a first and rest field.
; The empty list is just an empty object.
(define: (list->doof-list [vals : (Listof Value)]) : Value
  (foldr
   (lambda: ([val : Value] [obj : Value])
     (v-obj (list (field "first" val)
                  (field "rest" obj))))
   (v-obj empty)
   vals))

(define: (value->string [v : Value]) : String
  (match v
    [(v-str s) s]
    [(v-clos arg body env)
     (format "(lambda (~a) ...)" arg)]
    [(v-obj fields) "(obj ...)"]))
