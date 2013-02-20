#lang typed/racket

(provide
 (struct-out s-id)
 (struct-out s-str)
 (struct-out s-app)
 (struct-out s-lam)
 (struct-out s-prim)
 parse
 (rename-out [interp-expr interp]))

(require racket/match)

;(define-predicate sexp? Sexp))

(define-type Expr (U s-str s-id s-lam s-app s-prim))

(struct: s-str ([str : String]) #:transparent)
(struct: s-id ([id : Symbol]) #:transparent)
(struct: s-lam ([arg : Symbol] [body : Expr]) #:transparent)
(struct: s-app ([fun : Expr] [arg : Expr]) #:transparent)
(struct: s-prim ([name : Symbol] [arg : Expr]) #:transparent)

(define-type Value (U v-str v-clos))

(struct: v-str ([str : String]) #:transparent)
(struct: v-clos ([arg : Symbol] [body : Expr] [env : Env]) #:transparent)


(define-type Env (Listof (Pairof Symbol Value)))
(define mt-env empty)

(define extend-env cons)
(define bind cons)
(define: (lookup [id : Symbol] [env : Env]) : (Option Value)
  (cond
    [(empty? env) #f]
    [(equal? id (car (first env))) (cdr (first env))]
    [else (lookup id (rest env))]))

(define: (parse [s : Any]) : Expr
  (cond
    [(string? s) (s-str s)]
    [(symbol? s) (s-id s)]
    [(list? s)
     (match s
       [(list 'lambda (list arg) body)
        (if (symbol? arg) 
            (s-lam arg (parse body))
            (error "lambda arg not a symbol"))]
       [(list fun arg)
        (let ([fune (parse fun)])
          (if (and (s-id? fune) (is-prim? (s-id-id fune)))
              (s-prim (s-id-id fune) (parse arg))
              (s-app (parse fun) (parse arg))))]
       [_ (error "bad parse list")])]
    [else (error "bad parse")]))

(define: (interp-expr [e : Expr]) : String
  (match (interp e mt-env)
    [(v-str s) s]
    [else (error "interp-expr produced non-str")]))

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
     (let ([funv (interp fun env)]
           [argv (interp arg env)])
       (match funv
         [(v-clos carg cbody cenv)
          (interp cbody
                  (extend-env (bind carg argv) cenv))]
         [else (error "Can't apply non-function value")]))]))

(define (is-prim? id)
  (member id '(cat cat2)))

(define: (prim [name : Symbol] [arg : Value] [env : Env]) : Value
  (case name
    [(cat) (v-clos 's2
                   (s-prim '__cat2 (s-id 's2))
                   (extend-env (bind 's1 arg) mt-env))]
    [(__cat2) (let ([s (lookup 's1 env)])
                (if (and s (v-str? s) (v-str? arg))
                    (v-str (string-append (v-str-str s) (v-str-str arg)))
                    (error "prim error")))]
    [else (error "unknown prim")]))



