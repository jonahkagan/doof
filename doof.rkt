#lang typed/racket

(provide
 (struct-out s-id)
 (struct-out s-str)
 (struct-out s-app)
 (struct-out s-lam)
 (struct-out s-prim)
 (struct-out s-obj)
 (struct-out s-get)
 (struct-out s-ext)
 
 (struct-out v-str)
 (struct-out v-clos)
 (struct-out v-obj)
 
 parse
 (rename-out [interp-expr interp]))

(require racket/match)

;(define-predicate sexp? Sexp))

(define-type Expr (U s-str s-id s-lam s-app s-prim s-obj s-get s-ext))

(struct: s-str ([str : String]) #:transparent)
(struct: s-id ([id : Symbol]) #:transparent)
(struct: s-lam ([arg : Symbol] [body : Expr]) #:transparent)
(struct: s-app ([fun : Expr] [arg : Expr]) #:transparent)
(struct: s-prim ([name : Symbol] [arg : Expr]) #:transparent)
(struct: s-obj ([fields : (Listof (Pairof Expr Expr))]) #:transparent)
(struct: s-get ([obj : Expr] [field : Expr]) #:transparent)
(struct: s-ext ([obj : Expr] [field : Expr] [val : Expr]) #:transparent)

(define-type Value (U v-str v-clos v-obj))

(struct: v-str ([str : String]) #:transparent)
(struct: v-clos ([arg : Symbol] [body : Expr] [env : Env]) #:transparent)
(struct: v-obj ([fields : (HashTable String Value)]) #:transparent)

(define-type Env (Listof (Pairof Symbol Value)))
(define mt-env empty)

(define extend-env cons)
(define bind cons)
(define: (lookup [id : Symbol] [env : Env]) : (Option Value)
  (cond
    [(empty? env) #f]
    [(equal? id (car (first env))) (cdr (first env))]
    [else (lookup id (rest env))]))

; Really takes an Sexp, but typed/racket complained
(define: (parse [se : Any]) : Expr
  (cond
    [(string? se) (s-str se)]
    [(symbol? se) (s-id se)]
    [(list? se)
     (match se
       [(list 'lambda (list arg) body)
        (cond
          [(symbol? arg) (s-lam arg (parse body))]
          [else (error "lambda arg not a symbol")])]
       [(list 'obj fields ...)
        (s-obj (map (lambda (fe)
                      (match fe
                        [(list name ': value)
                         (cons (parse name) (parse value))]
                        [_ (error "bad parse field")]))
                    fields))]
       [(list 'get obj field)
        (s-get (parse obj) (parse field))]
       [(list 'ext obj (list field ': val))
        (s-ext (parse obj) (parse field) (parse val))]
       [(list fun arg)
        (match (parse fun)
          [(s-id fid)
           (cond
             [(is-prim? fid) (s-prim fid (parse arg))]
             [else (s-app (s-id fid) (parse arg))])]
          [exp (s-app exp (parse arg))])]
       [_ (error "bad parse list")])]
    [else (error "bad parse")]))


(define: (interp-expr [e : Expr]) : Value
  (interp e mt-env))

(define: (interp [e : Expr] [env : Env]) : Value
  (match e
    [(s-str s) (v-str s)]
    
    [(s-id id)
     (let ([v (lookup id env)])
       (if v v
           (error (string-append "Unbound id: " (symbol->string id)))))]
    
    [(s-prim name arg)
     (prim name (interp arg env) env)]
    
    [(s-lam arg body)
     (v-clos arg body env)]
    
    [(s-app fun arg)
     (match (interp fun env)
       [(v-clos carg cbody cenv)
        (interp cbody
                (extend-env (bind carg (interp arg env)) cenv))]
       [else (error "can't apply non-function value")])]
    
    [(s-obj fields)
     (v-obj (make-immutable-hash
             (map (lambda: ([f : (Pairof Expr Expr)])
                    (match (interp (car f) env)
                      [(v-str name) 
                       (cons name (interp (cdr f) env))]
                      [_ (error "field name must be a string")]))
                  fields)))]
    
    [(s-get obj field)
     (match (interp obj env)
       [(v-obj fields)
        (match (interp field env)
          [(v-str name)
           (hash-ref fields name)]
          [_ (error "get: field name must be a string")])]
       [_ (error "get: can't get from a non-object")])]
    
    [(s-ext obj field val)
     (match (interp obj env)
       [(v-obj fields)
        (match (interp field env)
          [(v-str name)
           (v-obj (hash-set fields name (interp val env)))]
          [_ (error "ext: field name must be a string")])]
       [_ (error "ext: can't extend a non-object")])]
    ))

; Primitives exposed to the language user
(define (is-prim? id)
  (member id '(cat names)))

(define: (prim [name : Symbol] [arg : Value] [env : Env]) : Value
  (case name
    [(cat) (v-clos 's2
                   (s-prim 'cat2 (s-id 's2))
                   (extend-env (bind 's1 arg) mt-env))]
    [(cat2) 
     (match (lookup 's1 env)
       [(v-str s1)
        (match arg
          [(v-str s2) 
           (v-str (string-append s1 s2))]
          [_ (error "cat: expected string as second arg")])]
       [_ (error "cat: expected string as first arg")])]
    
    [(names)
     (match arg
       [(v-obj fields)
        (list->doof-list 
         (map v-str 
              (map (lambda: ([p : (Pairof String Value)]) (car p))
                   (hash->list fields))))]
       [_ (error "names: expected object")])]
    
    [else (error "unknown prim")]))

(define: (list->doof-list [vals : (Listof Value)]) : Value
  (foldl
   (lambda: ([val : Value] [obj : Value])
     (v-obj (make-immutable-hash
             (list (cons "first" val)
                   (cons "rest" obj)))))
   (v-obj (make-immutable-hash '()))
   vals))