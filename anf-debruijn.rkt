#lang racket

;; input grammar:
;; exp = num | (- exp) | (let exp exp) | (+ exp exp)

;; output grammar:
;; aexp = (let cexp aexp) | cexp | atom
;; cexp = (- atom) | (+ atom atom) | atom
;; atom = num | var

(define (atom? e)
  (or (integer? e) ((list/c 'var natural?) e)))

(define expr/c
  (flat-rec-contract expr/c
  (or/c
   (list/c 'var natural?)
   integer?
   (list/c 'let expr/c expr/c)
   (list/c '+ expr/c expr/c)
   (list/c '- expr/c))))

(define ren/c (-> natural? natural?))

(define/contract ren-id
  ren/c
  (lambda (i) i))

(define/contract (scons a f)
  (-> natural? ren/c ren/c)
  (lambda (i)
    (if (zero? i) a (f (sub1 i)))))


(define/contract ren-shift
  ren/c
  add1)

(define/contract (ren-lift f)
  (-> ren/c ren/c)
  (scons 0 (compose ren-shift f)))

(define/contract (ren-exp ren a)
  (-> (-> natural? natural?) expr/c expr/c)
  (match a
    [`(var ,i)  `(var ,(ren i))]
    [(? integer?) a]
    [`(let ,e ,body)
     `(let ,(ren-exp ren e) ,(ren-exp (ren-lift ren) body))]
    [`(+ ,a ,b) `(+ ,(ren-exp ren a) ,(ren-exp ren b))]
    [`(- ,a) `(- ,(ren-exp ren a))]))

(define ctx/c (-> (-> natural? natural?) expr/c expr/c))

(define/contract (ctx-apply ctx e)
  (-> ctx/c expr/c expr/c )
  (ctx ren-id e))

(define/contract ctx-id
  ctx/c
  (lambda (_ e) e))

(define/contract (ctx-let body)
  (-> expr/c ctx/c)
  (lambda (xi e)
    `(let ,e ,(ren-exp (ren-lift xi) body))))

(define/contract (ctx-compose c0 c1)
  (-> ctx/c ctx/c ctx/c)
  (lambda (xi e)
    (c1 xi (c0 xi e))))

(define/contract (ctx-add rhs)
  (-> expr/c ctx/c)
  (lambda (xi e)
    `(+ ,e ,(ren-exp xi rhs))))

(define/contract (ctx-shift ctx)
  (-> ctx/c ctx/c)
  (lambda (xi e)
    (ctx (compose xi ren-shift) e)))


(define/contract (exp-anf-aexp e)
  (-> expr/c expr/c)
  (exp-anf-cexp e ctx-id))

(define/contract (exp-anf-atom e k)
  (-> expr/c ctx/c expr/c)
  (if (atom? e) (ctx-apply k e)
      (exp-anf-cexp
       e
       (ctx-let (ctx-apply (ctx-shift k) '(var 0))))))

(define/contract (exp-anf-cexp e k)
  (-> expr/c ctx/c expr/c)
  (match e
    [(list '+ a b)
     (exp-anf-atom
      a
      (lambda (xi0 va)
        (exp-anf-atom
         (ren-exp xi0 b)
         (lambda (xi1 vb)
           (k (compose xi1 xi0) `(+ ,(ren-exp xi1 va) ,vb))))))]
    [(list '- a)
     (exp-anf-atom
      a
      (lambda (xi va)
        (k xi `(- ,va))))]
    [`(let ,e ,body)
     (exp-anf-cexp
      e
      (lambda (xi va)
        `(let ,va
             ,(ren-exp (ren-lift xi)
                       (exp-anf-cexp body (ctx-shift k))))))]
    [(? atom?) (ctx-apply k e)]
    [_ (error (format "invalid syntactic form: ~a" e))]))

(provide exp-anf-aexp)
