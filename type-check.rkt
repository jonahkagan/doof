#lang typed/racket

(provide
 (rename-out [tc-expr tc]))

(require "ast.rkt")

(define: (a) (member? [elt : a] [lst : (Listof a)]) : Boolean
  (cond
    [(member elt lst) true]
    [else false]))

(: err (String Any * -> Nothing))
(define (err msg . vals)
  (apply error 'tc msg vals))

(define extend-env #{poly-extend-env @ TyValue})

(define str (tv-str (pat-all)))


(define: (subtype? [t1 : TyValue] [t2 : TyValue]) : Boolean
  (match* (t1 t2)
    [((tv-str s1) (tv-str s2))
     (subpat? s1 s2)]
    [((tv-arrow arg1 ret1) (tv-arrow arg2 ret2))
     (and (subtype? arg2 arg1) (subtype? ret1 ret2))]
    #|
    ; Width- and depth-based subtyping for objects
    [((tv-obj fields1) (tv-obj fields2))
     (andmap (lambda: ([f1 : t-field])
               (match (fields-get fields1 (t-field-name f1))
                 [(Some t2) (subtype? (t-field-value f1) t2)]  
                 [(None) false]))
             fields2)]
|#    
    [(_ _) false]))

(define: (subpat? [p1 : Pat] [p2 : Pat]) : Boolean
  (match (cons p1 p2)
    [(cons (pat-str p1) (pat-str p2)) (equal? p1 p2)]
    [(cons _ (pat-all)) true]
    [_ false]))

(define: (ty-interp [e : TyExpr] [env : TyEnv]) : TyValue
  (match e
    [(ts-str s) (tv-str s)]
    
    [(ts-id id)
     (match (lookup id env)
       [(None) (err "Unbound type id: ~a" id)]
       [(Some t) t])]
    
    [(ts-arrow arg ret)
     (tv-arrow (ty-interp arg env)
               (ty-interp ret env))]
    
    [(ts-lam arg body)
     (tv-clos arg body env)]
    
    [(ts-app fun arg)
     (match (ty-interp fun env)
       [(tv-clos carg cbody cenv)
        (ty-interp cbody
                   (extend-env (bind carg (ty-interp arg env)) cenv))]
       [_ (err "Can't apply non- type operator")])]
    ))

(define: (tc-expr [e : Expr]) : TyValue
  (tc e mt-env))

(define: (tc [e : Expr] [env : TyEnv]) : TyValue
  (match e
    [(s-str s) (tv-str (pat-str s))]
    
    [(s-id id)
     (match (lookup id env)
       [(None) (err "Unbound id: ~a" id)]
       [(Some t) t])]
    
    [(s-lam ty-e arg body)
     (define lam-ty (ty-interp ty-e mt-env))
     (match lam-ty
       [(tv-arrow argt rett)
        (define bodyt (tc body (extend-env (bind arg argt) env)))
        (cond
          [(subtype? bodyt rett) (tv-arrow argt rett)]
          [else (err "lambda type mismatch: ~a ~a" rett bodyt)])]
       ; We can't check whether the body has the proper type since
       ; we can't evaluate the closure until the lambda is applied.
       [(tv-clos _ _ _) lam-ty]
       [_ (err "(impossible) got a non-function type for lambda")])]
    
    
    [(s-app fun arg)
     (match (tc fun env)
       [(tv-arrow argt rett)
        (cond
          [(subtype? (tc arg env) argt) rett]
          [else (err "function type did not match arg type")])]
       ;[(tv-clos carg cbody cenv)
       [_ (err "can't apply non-function")])]
    
    [(s-cat e1 e2)
     (match (tc e1 env)
       [(tv-str s1)
        (match (tc e2 env)
          [(tv-str s2) (tv-str (pat-cat s1 s2))]
          [_ (err "cat: expected string as second arg")])]
       [_ (err "cat: expected string as first arg")])]
    #|    
    [(s-obj) (t-obj empty)]
    
    [(s-get obj field)
     (match (tc obj env)
       [(t-obj fields)
        (match (tc field env)
          [(t-str name)
           (match (fields-get fields name)
             [(Some t) t]
             [(None) (err "get: field not found: ~a" name)])]
          [_ (err "get: expected string for field name")])]
       [_ (err "get: expected obj")])]
    
    [(s-ext obj field val)
     (match (tc obj env)
       [(t-obj fields)
        (match (tc field env)
          [(t-str name) ; TODO check for singletons?
           (t-obj (fields-ext fields name (tc val env)))]
          [_ (err "ext: expected string for field name")])]
       [_ (err "ext: expected obj")])]
    
    [(s-fold fun acc obj)
     (match (tc obj env)
       [(tv-obj fields)
        (match (tc fun env)
          [(t-arrow namet (t-arrow valt (t-arrow acct rett)))
           (cond
             [(not (equal? namet str))
              (err "fold: first lambda must take Strings")]
             ; Should have a case ensuring second lambda accepts same type as obj values
             [(not (subtype? (tc acc env) acct))
              (err "fold: third lambda arg does not match type of initial accumulator")]
             [else rett])]
          [_ (err "fold: expected triply nested lambda")])]
       [_ (err "fold: expected object")])]
|#
    ))
#|
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
|#

(define pat-equal? equal?)
