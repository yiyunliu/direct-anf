#lang racket

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
