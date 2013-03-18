#lang typed/racket

(provide parse)

(require "ast.rkt")

; Really takes an Sexp, but typed/racket complained
(define: (parse [se : Any]) : Expr
  (cond
    [(string? se) (s-str se)]
    [(symbol? se) (s-id se)]
    [(list? se)
     (match se
       [`(lambda (,arg :: ,t-arg) -> ,t-body ,body)
        (cond
          [(symbol? arg)
           (s-lam (t-arrow (parse-type t-arg) (parse-type t-body))
                  arg (parse body))]
          [else (error "lambda arg not a symbol")])]
       
       [`(ty-lambda (,arg) ,body)
        (cond
          [(symbol? arg)
           (s-ty-lam arg (parse body))]
          [else (error "ty-lambda arg not a symbol")])]
       
       [`(obj) (s-obj)]
       
       [`(get ,obj ,field)
        (s-get (parse obj) (parse field))]
       
       [`(ext ,obj (,field : ,val))
        (s-ext (parse obj) (parse field) (parse val))]
       
       [`(fold ,fun ,acc ,obj)
        (s-fold (parse fun) (parse acc) (parse obj))]
       
       [`(cat ,s1 ,s2)
        (s-cat (parse s1) (parse s2))]
       
       [`(,fun ,arg)
        (s-app (parse fun) (parse arg))]
       
       [`(,fun @ ,arg)
        (s-ty-app (parse fun) (parse-type arg))]
       
       [_ (error "bad parse" se)])]
    [else (error "bad parse" se)]))

(define: (parse-type [se : Any]) : Type
  (cond
    [(string? se) (t-str (pat-str se))]
    [(symbol? se)
     (case se
       [(String) (t-str (pat-all))]
       [(Obj) (t-obj)]
       [else (t-id se)])]
    [(list? se)
     (match se
       [`(,arg -> ,ret)
        (t-arrow (parse-type arg) (parse-type ret))]
       [`(get ,obj ,field)
        (t-get (parse-type obj) (parse-type field))]
       [`(ext ,obj (,field : ,val))
        (t-ext (parse-type obj) (parse-type field) (parse-type val))]
       [_ (error "bad type parse" se)])]
    [else (error "bad type parse" se)]))