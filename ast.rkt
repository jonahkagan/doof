#lang typed/racket

(provide (all-defined-out))

(define-type (Opt a) (U None (Some a)))
(struct: (a) Some ([v : a]) #:transparent)
(struct: None () #:transparent)

;(define-predicate sexp? Sexp))

; Syntax
(define-type Expr (U s-str s-id s-lam s-app s-cat
                     s-obj s-get s-ext s-fold
                     s-ty-lam s-ty-app))

(struct: s-str ([str : String]) #:transparent)
(struct: s-id ([id : Symbol]) #:transparent)
(struct: s-lam ([type : t-arrow] [arg : Symbol] [body : Expr])
  #:transparent)
(struct: s-app ([fun : Expr] [arg : Expr]) #:transparent)
(struct: s-cat ([arg1 : Expr] [arg2 : Expr]))
(struct: s-obj () #:transparent)
(struct: s-get ([obj : Expr] [field : Expr]) #:transparent)
(struct: s-ext ([obj : Expr] [field : Expr] [val : Expr]) #:transparent)
(struct: s-fold ([fun : Expr] [acc : Expr] [obj : Expr]) #:transparent)
(struct: s-ty-lam ([arg : Symbol] [body : Expr]) #:transparent)
(struct: s-ty-app ([fun : Expr] [arg : Type]) #:transparent)

; Values
(define-type Value (U v-str v-clos v-obj))

(struct: v-str ([str : String]) #:transparent)
(struct: v-clos ([arg : Symbol] [body : Expr] [env : Env]) #:transparent)
(struct: v-obj ([fields : (Listof field)]) #:transparent)

(struct: field ([name : String] [value : Value]) #:transparent)

; Environments
(define-type (Envof a) (Listof (binding a)))
(struct: (a) binding ([id : Symbol] [value : a]) #:mutable)

(define-type Env (Envof Value))

; Types
(define-type Type (U t-str t-id t-arrow t-all))

(struct: t-str ([pat : Pat]) #:transparent)
(struct: t-id ([id : Symbol]) #:transparent)
(struct: t-arrow ([arg : Type] [ret : Type]) #:transparent)
(struct: t-all ([arg : Symbol] [body : Type]) #:transparent)

; Type environment
(define-type TyEnv (Envof Type))

; String patterns
(define-type Pat (U pat-str pat-cat pat-all))

(struct: pat-str [(str : String)] #:transparent)
(struct: pat-cat [(s1 : Pat) (s2 : Pat)] #:transparent)
(struct: pat-all () #:transparent)

; Environment helpers
(define mt-env empty)

(define: (a) (poly-extend-env [b : (binding a)] [env : (Envof a)]) : (Envof a)
  (cons b env))

(define bind binding)

(define: (a) (lookup [id : Symbol] [env : (Envof a)]) : (Opt a)
  (cond
    [(empty? env) (None)]
    [(equal? id (binding-id (first env)))
     (Some (binding-value (first env)))]
    [else (lookup id (rest env))]))