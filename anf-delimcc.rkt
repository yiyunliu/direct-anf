#lang racket
(require racket/control)

;; the correct delimited continuation primitive here is prompt + control
;; but in case I forget I'm spelling things out explicitly with prompt0 + control0

;; input grammar:
;; exp = num | (- exp) | (let ([x exp]) exp) | (+ exp exp)

;; output grammar:
;; aexp = (let ([x cexp]) aexp) | cexp | atom
;; cexp = (- atom) | (+ atom atom) | atom
;; atom = num | var

(define (atom? e)
  (or (integer? e) (symbol? e)))

(define (exp-anf-aexp e)
  (prompt0 (exp-anf-cexp e)))

(define (exp-anf-atom e)
  (if (atom? e) e
      (let ([x (gensym 'x)])
        (control0 k `(let ([,x ,(exp-anf-cexp e)]) ,(k x))))))

#|
it's quite confusing but for all recursive calls within exp-anf-cexp,
it's safe to assume that the result is a cexp.
the let bindings are all appended to the continuation and only get added
after the final top-level call to exp-anf-cexp
|#
(define (exp-anf-cexp e)
  (match e
    [(list '+ a b)
     (list '+ (exp-anf-atom a) (exp-anf-atom b))]
    [(list '- a)
     (list '- (exp-anf-atom a))]
    [`(let ([,x ,e]) ,body)
     (control0 k (prompt0 `(let ([,x ,(exp-anf-cexp e)]) ,(k (exp-anf-cexp body)))))]
    [(? integer?) e]
    [(? symbol?) e]
    [_ (error (format "invalid syntactic form: ~a" e))]))

(provide exp-anf-aexp)
