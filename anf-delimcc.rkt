#lang racket
(require racket/control)

;; input grammar:
;; exp = num | (- exp) | (let ([var exp]) exp) | (+ exp exp)

;; output grammar:
;; aexp = (let ([var cexp]) aexp) | cexp | atom
;; cexp = (- atom) | (+ atom atom) | atom
;; atom = num | var

(define (atom? e)
  (or (integer? e) (symbol? e)))

(define (exp-anf-aexp e)
  (prompt0 (exp-anf-cexp e)))

(define (exp-anf-atom e)
  (if (atom? e) e
      (let ([x (gensym 'x)])
        (control0 k (prompt0 `(let ([,x ,(exp-anf-cexp e)]) ,(prompt0 (k x))))))))

#|
it's quite confusing but for all recursive calls within exp-anf-cexp,
it's safe to assume that the result is a cexp.
the let bindings are all appended to the continuation and only get added
after the final top-level call to exp-anf-cexp.
It might help to try typing the prompt tag
|#
(define (exp-anf-cexp e)
  (match e
    [(list '+ a b)
     (list '+ (exp-anf-atom a) (exp-anf-atom b))]
    [(list '- a)
     (list '- (exp-anf-atom a))]
    [`(let ([,x ,e]) ,body)
     (control0 k (prompt0 `(let ([,x ,(exp-anf-cexp e)]) ,(prompt0 (k (exp-anf-cexp body))))))]
    [(? integer?) e]
    [(? symbol?) e]
    [_ (error (format "invalid syntactic form: ~a" e))]))

(provide exp-anf-aexp)
