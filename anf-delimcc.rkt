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
  (let ([v (exp-anf-cexp e)])
    (control0 k (prompt0
                 (cond
                   [(atom? v) (k v)]
                   [else
                    (let ([x (gensym 'x)])
                      `(let ([,x ,v]) ,(prompt0 (k x))))])))))

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
    [(list 'if a b c)
     (list 'if (exp-anf-cexp a)
           (prompt0 (exp-anf-cexp b))
           (prompt0 (exp-anf-cexp c)))]
    [`(let ([,x ,e]) ,body)
     (let ([v (exp-anf-cexp e)])
       (control0 k `(let ([,x ,v]) ,(prompt0 (k (exp-anf-cexp body))))))]
    [(? integer?) e]
    [(? symbol?) e]
    [_ (error (format "invalid syntactic form: ~a" e))]))



(struct Effect (k) #:transparent)
(struct Put Effect (v) #:transparent)
(struct Get Effect () #:transparent)
(define state-tag (make-continuation-prompt-tag))
(define (put v)
  (control0-at state-tag k (Put k v)))
(define (get)
  (control0-at state-tag k (Get k)))
(define (with-state ))

(define (commute-if-cexpr a)
  (match a
    [`(if ,a ,b ,c)
     (control0
      k
      (let ([l0 (gensym)]
            [l1 (gensym)]
            [b0 (prompt0 (k (commute-if-aexpr b)))]
            [c0 (prompt0 (k (commute-if-aexpr c)))])
        (hash-set! cfg l0 b0)
        (hash-set! cfg l1 c0)
        (prompt0
         (let ([v (commute-if-cexpr a)])
           `(if ,v (goto ,l0) (goto ,l1))))))]
    [(? number?) a]
    [_ a]))

(define (commute-if-aexpr a)
  [match a
    [`(let ([,x ,a]) ,e)
     (control0
      k
      (let ([l (gensym)]
            [e0 (prompt0 (k (commute-if-aexpr e)))])
        (hash-set! cfg l e0)
        (prompt0
         (let ([v (commute-if-cexpr a)])
           `(let ([,x ,v]) (goto ,l))))))]
    [_ (commute-if-cexpr a)]])

(provide exp-anf-aexp)
