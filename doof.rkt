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
 (struct-out s-if-empty)
 (struct-out s-rec)
 
 (struct-out v-str)
 (struct-out v-clos)
 (struct-out v-obj)
 
 parse
 (rename-out [interp-expr interp]))

(require racket/match)

(define-type (Opt a) (U None (Some a)))
(struct: (a) Some ([v : a]) #:transparent)
(struct: None () #:transparent)

;(define-predicate sexp? Sexp))

(define-type Expr (U s-str s-id s-lam s-app s-prim s-obj s-get s-ext
                     s-if-empty s-rec))

(struct: s-str ([str : String]) #:transparent)
(struct: s-id ([id : Symbol]) #:transparent)
(struct: s-lam ([arg : Symbol] [body : Expr]) #:transparent)
(struct: s-app ([fun : Expr] [arg : Expr]) #:transparent)
(struct: s-prim ([name : Symbol] [arg : Expr]) #:transparent)
(struct: s-obj () #:transparent)
(struct: s-get ([obj : Expr] [field : Expr]) #:transparent)
(struct: s-ext ([obj : Expr] [field : Expr] [val : Expr]) #:transparent)
(struct: s-if-empty ([obj : Expr] [then : Expr] [else : Expr])
  #:transparent)
(struct: s-rec ([name : Symbol] [fun : s-lam] [rest : Expr]) #:transparent)

(define-type Value (U v-str v-clos v-obj))

(struct: v-str ([str : String]) #:transparent)
(struct: v-clos ([arg : Symbol] [body : Expr] [env : Env]) #:transparent)
(struct: v-obj ([fields : (HashTable String Value)]) #:transparent)

(define hash make-immutable-hash)

(define-type Env (Listof binding))
(struct: binding ([id : Symbol] [value : Value]) #:mutable)

(define mt-env empty)
(define: (extend-env [b : binding] [env : Env]) : Env
  (cons b env))
(define bind binding)
(define: (lookup [id : Symbol] [env : Env]) : (Opt Value)
  (cond
    [(empty? env) (None)]
    [(equal? id (binding-id (first env)))
     (Some (binding-value (first env)))]
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
       
       [(list 'obj) (s-obj)]
       
       [(list 'get obj field)
        (s-get (parse obj) (parse field))]
       
       [(list 'ext obj (list field ': val))
        (s-ext (parse obj) (parse field) (parse val))]
       
       [(list 'if-empty obj then else)
        (s-if-empty (parse obj) (parse then) (parse else))]
       
       [(list (list 'def-rec (list name arg) body) rest)
        (cond
          [(not (symbol? name)) (error "rec: name must be a symbol")]
          [(not (symbol? arg)) (error "rec: arg must be a symbol")]
          [else (s-rec name (s-lam arg (parse body)) (parse rest))])]

       [(list fun arg)
        (match (parse fun)
          [(s-id fid)
           (cond
             [(is-prim? fid) (s-prim fid (parse arg))]
             [else (s-app (s-id fid) (parse arg))])]
          [exp (s-app exp (parse arg))])]
       [_ (error "bad parse list")])]
    [else (error "bad parse")]
    ))


(define: (interp-expr [e : Expr]) : Value
  (interp e mt-env))

(define: (interp [e : Expr] [env : Env]) : Value
  (match e
    [(s-str s) (v-str s)]

    [(s-id id)
     (match (lookup id env)
       [(None) (error (string-append "Unbound id: " (symbol->string id)))]
       [(Some v) v])]

    [(s-prim name arg)
     (prim name (interp arg env) env)]

    [(s-lam arg body)
     (v-clos arg body env)]
    
    [(s-rec name lam rest)
     (define rec-binding (bind name (v-str "dummy")))
     (define rec-env (extend-env rec-binding env))
     (define clos (interp lam rec-env))
     (match clos
       [(v-clos _ _ _)
        (set-binding-value! rec-binding clos)
        (interp rest rec-env)]
       [_ (error "rec: somehow got a non-function")])]
       

    [(s-app fun arg)
     (match (interp fun env)
       [(v-clos carg cbody cenv)
        (interp cbody
                (extend-env (bind carg (interp arg env)) cenv))]
       [else (error "can't apply non-function value")])]

    [(s-obj)
     (v-obj (hash empty))]

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
       [_ (error "ext: expected object")])]
    
    [(s-if-empty obj then else)
     (match (interp obj env)
       [(v-obj fields) 
        (cond
          [(equal? fields (hash empty))
           (interp then env)]
          [else (interp else env)])]
       [_ (error "if-empty: expected object")])]
    ))

; Primitives exposed to the language user
(define (is-prim? id)
  (member id '(cat names)))

(define: (prim [name : Symbol] [arg : Value] [env : Env]) : Value
  (case name
    ; Bit of a trick to make cat a one arg lambda that will be curried
    ; with the first string. We create a closure and bind the first string
    ; in its env as s1. Then we call cat2, which knows to look up s1 in the
    ; env which is passed into the call to prim.
    [(cat) (v-clos 's2
                   (s-prim 'cat2 (s-id 's2))
                   (extend-env (bind 's1 arg) mt-env))]
    [(cat2) 
     (match (lookup 's1 env)
       [(Some (v-str s1))
        (match arg
          [(v-str s2)
           (v-str (string-append s1 s2))]
          [_ (error "cat: expected string as second arg")])]
       [_ (error "cat: expected string as first arg")])]
    
    [(names)
     (match arg
       [(v-obj fields)
        (list->doof-list (map v-str (hash-keys fields)))]
       [_ (error "names: expected object")])]
    
    [else (error "unknown prim")]))

; Constructs an object representing a list that has a first and rest field.
; The empty list is just an empty object.
(define: (list->doof-list [vals : (Listof Value)]) : Value
  (foldl
   (lambda: ([val : Value] [obj : Value])
     (v-obj (hash
             (list (cons "first" val)
                   (cons "rest" obj)))))
   (v-obj (hash empty))
   vals))

(define: (value->string [v : Value]) : String
  (match v
    [(v-str s) s]
    [(v-clos arg body env)
     (format "(lambda (~a) ...)" arg)]
    [(v-obj fields) "(obj ...)"]))