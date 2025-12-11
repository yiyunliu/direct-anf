#lang racket

;; the correct delimited continuation primitive here is reset + shift
;; I'm spelling things out explicitly with prompt0 + control0 to avoid confusion

;; input grammar:
;; exp = num | (- exp) | (let ([var exp]) exp) | (+ exp exp)

;; output grammar:
;; aexp = (let ([var cexp]) aexp) | cexp | atom
;; cexp = (- atom) | (+ atom atom) | atom
;; atom = num | var

(define (atom? e)
  (or (integer? e) (symbol? e)))

(define (exp-anf-aexp e)
  (exp-anf-cexp e identity))

(define (exp-anf-atom e k)
  (if (atom? e) (k e)
      (exp-anf-cexp
       e
       (let ([x (gensym 'x)])
         (lambda (v)
           `(let ([,x ,v])
              ,(k x)))))))

#|
it's quite confusing but for all recursive calls within exp-anf-cexp,
it's safe to assume that the result is a cexp.
the let bindings are all appended to the continuation and only get added
after the final top-level call to exp-anf-cexp
|#
(define (exp-anf-cexp e k)
  (match e
    [(list '+ a b)
     (exp-anf-atom a (lambda (va)
                       (exp-anf-atom
                        b (lambda (vb)
                            (k `(+ ,va ,vb))))))]
    [(list '- a)
     (list (exp-anf-atom
            a
            (lambda (va)
              (k `(- ,va)))))]
    [`(let ([,x ,e]) ,body)
     (exp-anf-cexp
      e
      (lambda (va)
        `(let ([,x ,va])
           ,(exp-anf-cexp body k))))]
    [(? integer?) (k e)]
    [(? symbol?) (k e)]
    [_ (error (format "invalid syntactic form: ~a" e))]))

(provide exp-anf-aexp)
