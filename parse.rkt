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
           (s-lam (ts-arrow (parse-type t-arg) (parse-type t-body))
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
       
       [`(,fun ! ,arg)
        (s-ty-app (parse fun) (parse-type arg))]
       
       [_ (error "bad parse" se)])]
    [else (error "bad parse" se)]))

(define: (parse-type [se : Any]) : TyExpr
  (cond
    [(string? se) (ts-str (parse-pat se))]
    [(symbol? se)
     (case se
       [(String) (ts-str (pat-all))]
       [else (ts-id se)])]
    [(list? se)
     (match se
       [`(,arg -> ,ret)
        (ts-arrow (parse-type arg) (parse-type ret))]
#|
[`(Obj (,names : ,fields) ...)
        (cond
          [(andmap string? names)
           (t-obj (map t-field
                       (map parse-pat names)
                       (map parse-type fields)))]
          [else (error "type field names must be pattern strings")])]
|#
       [_ (error "bad type parse" se)])]
    [else (error "bad type parse" se)]))

(define: (parse-pat [str : String]) : Pat
  (pat-str str))