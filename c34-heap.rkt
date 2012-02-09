#lang r6rs

;; 3.4.2 Translation and 3.4.3 Evaluation

;; aux macro

(define-syntax record-case-bind
  (syntax-rules ()
    ((record-case-bind val () body)
     (begin . body))
    ((record-case-bind val (var . rest) body)
     (let ((var (car val)))
       (record-case-bind (cdr val) rest body)))))

(define-syntax record-case
  (syntax-rules (else)
    ((record-case expr) #f)
    ((record-case expr (else . body) . rest)
     (begin . body))
    ((record-case expr ((key . binds) . body) . rest)
     (let ((val expr))
       (if (eq? (car val) 'key)
	   (record-case-bind (cdr val) binds body)
	   (record-case val . rest))))))


;; registers
;; a: the accumulator (last value computed by a value-returning operation)
;; x: the next expression (= program counter)
;; e: the current environment
;; r: the current value rib (actual arguments)
;; s: the current stack (call frame)

;; aux
(define (tail? next)
  (eq? (car next) ':return))

(define (lookup var e)
  (let nxtrib ((e e))
    (let nxtelt ((vars (caar e))
		 (vals (cdar e)))
      (cond
       ((null? vars) (nxtrib (cdr e)))
       ((eq? (car vars) var) vals)
       (else (nxtelt (cdr vars) (cdr vals)))))))

(define (replace-var var env a)
  (define (nxtelt vars vals pvars pvals)
    (cond
     ((null? vars)
      (cons (car env)
	    (replace-var var (cdr env) a)))
     ((eq? (car vars) var)
      (cons
       (cons (append (reverse pvars) vars)
	     (append (reverse pvals)
		     (cons a (cdr vals))))
       (cdr env)))
     (else (nxtelt (cdr vars)
		   (cdr vals)
		   (cons (car vars) pvars)
		   (cons (car vals) pvals)))))
  (nxtelt (caar env) (cdar env) '() '()))

(define (closure body e vars)
  `(,body ,e ,vars))

(define (continuation s)
  (closure `(:nuate ,s v) '() '(v)))

(define (call-frame x e r s)
  `(,x ,e ,r ,s))

(define (extend e vars vals)
  `((,vars . ,vals) . ,e))

;; 3.4.2 Translation

(define (compile x next)
  (cond
   [(symbol? x)
    `(:refer ,x ,next)]
   [(pair? x)
    (record-case x
     [(quote obj)
      `(:constant ,obj ,next)]
     [(lambda vars body)
      `(:close ,vars ,(compile body '(:return)) ,next)]
     [(if test then els)
      (let ((thenc (compile then next))
	    (elsec (compile els next)))
	(compile test `(:test ,thenc ,elsec)))]
     [(set! var x)
      (compile x `(:assign ,var ,next))]
     [(call/cc x)
      (let ((c `(:conti (:argument ,(compile x '(:apply))))))
	(if (tail? next)
	    c
	    `(:frame ,next ,c)))]
     [else
      (let loop ((args (cdr x))
		 (c (compile (car x) '(:apply))))
	(if (null? args)
	    (if (tail? next)
		c
		`(:frame ,next ,c))
	    (loop (cdr args)
		  (compile (car args)
			   `(:argument ,c)))))])]
   [else
    `(:constant ,x ,next)]))

;; 3.4.3 Evaluation

(define (VM a expr env rib stack)
  (record-case expr
   [(:halt) a]
   [(:refer var x)
    (VM (car (lookup var env)) x env rib stack)]
   [(:constant obj x)
    (VM obj x env rib stack)]
   [(:close vars body x)
    (VM (closure body env vars) x env rib stack)]
   [(:test then els)
    (VM a (if a then els) env rib stack)]
   [(:assign var x)
    (VM a x (replace-var var env a) rib stack)]
   [(:conti x)
    (VM (continuation stack) x env rib stack)]
   [(:nuate s var)
    (VM (car (lookup var env)) '(:return) env rib s)]
   [(:frame ret x)
    (VM a x env '() (call-frame ret env rib stack))]
   [(:argument x)
    (VM a x env (cons a rib) stack)]
   [(:apply)
    (let ((body (list-ref a 0))
	  (e (list-ref a 1))
	  (vars (list-ref a 2)))
      (VM a body (extend e vars rib) '() stack))]
   [(:return)
    (let ((x (list-ref stack 0))
	  (e (list-ref stack 1))
	  (r (list-ref stack 2))
	  (s (list-ref stack 3)))
      (VM a x e r s))]))

(define (evaluate x)
  (VM '() (compile x '(:halt)) '() '() '()))

