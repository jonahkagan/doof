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

(define pat-equal? equal?)

(define str (t-str (pat-all)))

(define: (subtype? [t1 : Type] [t2 : Type]) : Boolean
    (match* (t1 t2)
      [((t-str s1) (t-str s2))
       (subpat? s1 s2)]
      [((t-id id1) (t-id id2))
       (equal? id1 id2)]
      [((t-arrow arg1 ret1) (t-arrow arg2 ret2))
       (and (subtype? arg2 arg1) (subtype? ret1 ret2))]
      [((t-obj) (t-obj)) true]
      ; Width- and depth-based subtyping for objects
      [((t-ext _ _ _) (t-ext _ _ _))
       (define fields1 (ty-fields-list t1))
       (andmap (lambda: ([f2 : (Pairof Pat Type)])
                 (match (ty-fields-list-lookup (car f2) fields1)
                   [(Some t2) (subtype? (cdr f2) t2)]
                   [(None) false]))
               (ty-fields-list t2))]
      [(_ _) (equal? t1 t2)]))

(define: (subpat? [p1 : Pat] [p2 : Pat]) : Boolean
  (match (cons p1 p2)
    [(cons (pat-str p1) (pat-str p2)) (equal? p1 p2)]
    [(cons _ (pat-all)) true]
    [_ false]))
    
(define: (ty-fields-list [t : Type]) : (Listof (Pairof Pat Type))
  (match t
    [(t-obj) empty]
    [(t-ext obj (t-str pat) val)
     (cons (cons pat val)
           (ty-fields-list obj))]))

(define: (ty-fields-list-lookup [name : Pat] [fields : (Listof (Pairof Pat Type))])
  : (Opt Type)
  (cond
    [(empty? fields) (None)]
    [(pat-equal? name (car (first fields))) (Some (cdr (first fields)))]
    [else (ty-fields-list-lookup name (rest fields))]))

#|
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
|#

(define: (ty-fv [t : Type]) : (Listof Symbol)
  (match t
    [(t-str _) empty]
    [(t-id id) (list id)]
    [(t-arrow arg ret) (append (ty-fv arg) (ty-fv ret))]
    [(t-all id body) (remove* '(id) (ty-fv body))]))

(define: (ty-subst [t : Type] [id : Symbol] [body : Type]) : Type
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
       [(member? id1 (ty-fv t)) (err "trying to subst a type with a free var into a ty-lambda with that same var - just don't do it!")]
       ;(ty-subst t id (ty-alpha-conv body))]
       [else (t-all id1 (ty-subst t id t2))])]
    ))

(define: (ty-get-field [obj : Type] [name : Type]) : Type
  (match obj
    [(t-obj) (err "get: field not found on empty obj" )]
    [(t-ext obj-ext name-ext val-ext)
     (match* (name name-ext)
       [((t-str name) (t-str name-ext))
        (cond
          [(pat-equal? name name-ext) val-ext]
          [else (ty-get-field obj-ext (t-str name))])]
       [(_ _) (err "get: expected string type for field name")])]
    [_ (err "get: expected object type")]))

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
    
    [(s-obj) (t-obj)]
    
    [(s-get obj name)
     (ty-get-field (tc obj env ty-vars) (tc name env ty-vars))]
    
    [(s-ext obj name val)
     (t-ext (tc obj env ty-vars)
            (tc name env ty-vars)
            (tc val env ty-vars))]
    
    #|
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


