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
       [`(lambda (,arg ,arg-t) -> ,ret-t ,body)
        (cond
          [(symbol? arg) (s-lam (t-arrow (parse-type arg-t)
                                         (parse-type ret-t))
                                arg (parse body))]
          [else (error "lambda arg not a symbol")])]
       
       [`(obj) (s-obj)]
       
       [`(get ,obj ,field)
        (s-get (parse obj) (parse field))]
       
       [`(ext ,obj (,field : ,val))
        (s-ext (parse obj) (parse field) (parse val))]
       
       [`(if-empty ,obj ,then ,else)
        (s-if-empty (parse obj) (parse then) (parse else))]
       
       #;[`((def-rec (,name ,arg) ,body) ,rest)
          (cond
            [(not (symbol? name)) (error "rec: name must be a symbol")]
            [(not (symbol? arg)) (error "rec: arg must be a symbol")]
            [else (s-rec name (s-lam arg (parse body)) (parse rest))])]
       
       [`(,fun ,arg) (s-app (parse fun) (parse arg))]
       
       [_ (error "bad parse" se)])]
    [else (error "bad parse" se)]))

(define: (parse-type [se : Any]) : Type
  (cond
    [(string? se) (t-str (parse-pat se))]
    [(symbol? se)
     (case se
       [(String) (t-str (pat-all))]
       [else (error "bad type parse symbol" se)])]
    [(list? se)
     (match se
       [`(,arg -> ,ret)
        (t-arrow (parse-type arg) (parse-type ret))]
       [`(Obj (,names : ,fields) ...)
        (cond
          [(andmap string? names)
           (t-obj (map t-field
                       (map parse-pat names)
                       (map parse-type fields)))]
          [else (error "type field names must be pattern strings")])]
       [_ (error "bad type parse" se)])]
    [else (error "bad type parse" se)]))

(define: (parse-pat [str : String]) : Pat
  (pat-str str))