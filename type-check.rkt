#lang typed/racket

(provide
 (rename-out [tc-expr tc]))

(require "ast.rkt")

(define: (a) (member? [elt : a] [lst : (Listof a)]) : Boolean
  (cond
    [(member elt lst) true]
    [else false]))

(define str (t-str (pat-all)))

(define: (subtype? [t1 : Type] [t2 : Type]) : Boolean
  (match (cons t1 t2)
    [(cons (t-str s1) (t-str s2))
     (subpat? s1 s2)]
    [(cons (t-arrow arg1 ret1) (t-arrow arg2 ret2))
     (and (subtype? arg2 arg1) (subtype? ret1 ret2))]
    ; Width-based subtyping for objects
    ; (could add depth also, but not necessary yet)
    [(cons (t-obj fields1) (t-obj fields2))
     (andmap (lambda: ([f : t-field]) (member? f fields1))
             fields2)]
    [_ false]))

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
    
    [(s-obj) (t-obj empty)]
    
    [(s-get obj field)
     (match (tc obj env)
       [(t-obj fields)
        (match (tc field env)
          [(t-str name)
           (match (fields-get fields name)
             [(Some t) t]
             [(None) (error "get: field not found" name)])]
          [_ (error "get: expected string for field name")])]
       [_ (error "get: expected obj")])]
    
    [(s-ext obj field val)
     (match (tc obj env)
       [(t-obj fields)
        (match (tc field env)
          [(t-str name) ; TODO check for singletons?
           (t-obj (fields-ext fields name (tc val env)))]
          [_ (error "ext: expected string for field name")])]
       [_ (error "ext: expected obj")])]
    
    ;[(s-if-empty obj then else) ...]
    ))

; For now, copied these from interp. Object get/ext will be well-typed
; only when the given field name is a singleton string.
(define: (fields-get [fields : (Listof t-field)] [name : Pat])
  : (Opt Type)
  (cond
    [(empty? fields) (None)]
    [(pat-equal? name (t-field-name (first fields)))
     (Some (t-field-value (first fields)))]
    [else (fields-get (rest fields) name)]))

(define: (fields-ext [fields : (Listof t-field)] [name : Pat]
                     [val : Type]) : (Listof t-field)
  (cons (t-field name val)
        (filter (lambda: ([f : t-field])
                  (not (pat-equal? (t-field-name f) name)))
                fields)))

(define pat-equal? equal?)


(define (tc-prim name)
  (case name
    [(cat) (t-arrow str (t-arrow str str))]
    [(cat2) (t-arrow str str)]
    ;[(names) ...]
    [else (error "unknown prim" name)]))