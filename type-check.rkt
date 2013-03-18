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

(define extend-env #{poly-extend-env @ Type})

(define str (t-str (pat-all)))


(define: (subtype? [t1 : Type] [t2 : Type]) : Boolean
  (match* (t1 t2)
    [((t-str s1) (t-str s2))
     (subpat? s1 s2)]
    [((t-id id1) (t-id id2))
     (equal? id1 id2)]
    [((t-arrow arg1 ret1) (t-arrow arg2 ret2))
     (and (subtype? arg2 arg1) (subtype? ret1 ret2))]
    #|
    ; Width- and depth-based subtyping for objects
    [((t-obj fields1) (t-obj fields2))
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

#|
(define: (ty-interp [e : TyExpr] [env : TyEnv]) : Type
  (match e
    [(ts-str s) (tv-str s)]
    
    [(ts-id id)
     (match (lookup id env)
       [(None) (err "Unbound type id: ~a" id)]
       [(Some t) t])]
    
    [(ts-arrow arg ret)
     (tv-arrow (ty-interp arg env)
               (ty-interp ret env))]
    
    ;[(ts-lam arg body)
    ; (tv-clos arg body env)]
    
    ;[(ts-app fun arg)
    ; (ty-app (ty-interp fun env) (ty-interp arg env))]
    ))
|#
(define n 0)
(define: (replace [id : Symbol] [new-id : Symbol] [t : Type]) : Type
  (match t
    [(t-str _) t]
    [(t-id id1) 
     (cond
       [(equal? id1 id) (t-id new-id)]
       [else t])]
    [(t-arrow arg ret)
     (t-arrow (replace id new-id arg) (replace id new-id ret))]
    [(t-all x body)
     (cond
       [(equal? x id) body]
       [else (t-all x (replace id new-id body))])]))
(define ty-alpha-conv
  (lambda: ([t : Type])
    (match t
      [(t-all id t2)
       (set! n (add1 n))
       (define new-id (string->symbol (format "alpha~a" n)))
       (t-all new-id (replace id new-id t2))]
      [_ (error "alpha-conv only for t-all")])))

(define: (ty-fv [t : Type]) : (Listof Symbol)
  (match t
    [(t-str _) empty]
    [(t-id id) (list id)]
    [(t-arrow arg ret) (append (ty-fv arg) (ty-fv ret))]
    [(t-all id body) (remove* '(id) (ty-fv body))]))

(define: (ty-subst [t : Type] [id : Symbol] [body : Type]) : Type
  #|(cond
    [(not (or (empty? (ty-fv body))
          (equal? (ty-fv body) (list id))))
          (err "can't subst into non-closed type")]
    [else
|#
  (match body
    [(t-str _) body]
    
    [(t-id id1)
     (cond
       [(equal? id1 id) t]
       [else body])]
    
    [(t-arrow arg ret) (t-arrow (ty-subst t id arg)
                                (ty-subst t id ret))]
    
    [(t-all id1 t2)
     (cond
       [(equal? id1 id) body]
       [(member? id1 (ty-fv t)) (ty-subst t id (ty-alpha-conv body))]
       [else (t-all id1 (ty-subst t id t2))])]
    ))

(define: (tc-expr [e : Expr]) : Type
  (tc e mt-env empty))

(define: (tc [e : Expr] [env : TyEnv] [ty-vars : (Listof Symbol)]) : Type
  (match e
    [(s-str s) (t-str (pat-str s))]
    
    [(s-id id)
     (match (lookup id env)
       [(None) (err "Unbound id: ~a" id)]
       [(Some t) t])]
    
    [(s-lam ty arg body)
     (match ty
       [(t-arrow argt rett)
        (define bodyt (tc body (extend-env (bind arg argt) env) ty-vars))
        (cond
          [(subtype? bodyt rett) (t-arrow argt rett)]
          [else (err "lambda type mismatch: ~a ~a" rett bodyt)])]
       [_ (err "(impossible) got a non-function type for lambda")])]
    
    [(s-app fun arg)
     (match (tc fun env ty-vars)
       [(t-arrow argt rett)
        (define given-argt (tc arg env ty-vars))
        (cond
          [(subtype? given-argt argt) rett]
          [else (err "function type did not match arg type: ~a ~a"
                     argt given-argt)])]
       [_ (err "can't apply non-function")])]
    
    [(s-ty-lam arg body)
     (t-all arg (tc body env (cons arg ty-vars)))]
    
    [(s-ty-app fun arg)
     (match (tc fun env ty-vars)
       [(t-all arg-id body-ty)
        (match arg
          [(t-id id)
           (cond 
             [(member? id ty-vars) (ty-subst arg arg-id body-ty)]
             [else (err "unbound type var: ~a" id)])]
          [_ (ty-subst arg arg-id body-ty)])]
       [_ (err "can't apply non-universal to type")])]
    
    [(s-cat e1 e2)
     (match (tc e1 env ty-vars)
       [(t-str s1)
        (match (tc e2 env ty-vars)
          [(t-str s2) (t-str (pat-cat s1 s2))]
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
       [(t-obj fields)
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
