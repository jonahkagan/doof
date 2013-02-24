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
       [(list 'lambda (list arg arg-t) -> ret-t body)
        (cond
          [(symbol? arg) (s-lam (t-arrow (parse-type arg-t)
                                         (parse-type ret-t))
                                arg (parse body))]
          [else (error "lambda arg not a symbol")])]
       
       [(list 'obj) (s-obj)]
       
       [(list 'get obj field)
        (s-get (parse obj) (parse field))]
       
       [(list 'ext obj (list field ': val))
        (s-ext (parse obj) (parse field) (parse val))]
       
       [(list 'if-empty obj then else)
        (s-if-empty (parse obj) (parse then) (parse else))]
       
       #;[(list (list 'def-rec (list name arg) body) rest)
        (cond
          [(not (symbol? name)) (error "rec: name must be a symbol")]
          [(not (symbol? arg)) (error "rec: arg must be a symbol")]
          [else (s-rec name (s-lam arg (parse body)) (parse rest))])]
       
       [(list fun arg)
        (match (parse fun)
          [(s-id fid)
           (cond
             [(member fid prim-names) (s-prim fid (parse arg))]
             [else (s-app (s-id fid) (parse arg))])]
          [exp (s-app exp (parse arg))])]
       [_ (error "bad parse list" se)])]
    [else (error "bad parse" se)]
    ))

(define: (parse-type [se : Any]) : Type
  (cond
    [(string? se) (t-str (parse-pat se))]
    [(symbol? se)
     (case se
       [(String) (t-str (pat-all))]
       [else (error "bad type parse symbol" se)])]
    [(list? se)
     (match se
       [(list arg '-> ret)
        (t-arrow (parse-type arg) (parse-type ret))]
       [(list 'Obj (list names ': fields) ...)
        (cond
          [(andmap string? names)
           (t-obj (map t-field
                       (map parse-pat names)
                       (map parse-type fields)))]
          [else (error "type field names must be pattern strings")])]
       [_ (error "bad type parse list" se)])]
    [else (error "bad type parse" se)]))

(define: (parse-pat [str : String]) : Pat
  (pat-str str))