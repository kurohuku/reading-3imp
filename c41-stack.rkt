#lang r6rs

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


;; call frame
;; 1. the frame pointer holding the address of the call frame of the suspended call (dynamic link)
;; 2. the arguments to the called function
;; 3. the return address (the next expression after the call) and
;; 4. the frame pointer holding the address of the call frame of the next outer scope of the claled function
;;    (static link)

;;; static link (pushed last)
;;; first argument
;;; ...
;;; last argument
;;; next-expression (return address)
;;; dynamic link (pushed first)

(define (functional body env)
  `(,body ,env))

(define stack (make-vector 1000))
(define (push obj pos)
  (vector-set! stack pos obj)
  (+ pos 1))

(define (index s i)
  (vector-ref stack (- (- s i) 1)))
(define (index-set! s i v)
  (vector-set! stack (- (- s i) 1) v))

(define (compile-lookup var env ret)
  (let nxtrib ([e env] [rib 0])
    (let nxtelt ([vars (car e)] [elt 0])
      (cond
       ((null? vars) (nxtrib (cdr e) (+ rib 1)))
       ((eq? (car vars) var) (ret rib elt))
       (else (nxtelt (cdr vars) (+ elt 1)))))))

(define (extend env rib)
  (cons rib env))




;; 3.1.5 Translation
;; closure, tail-call optimization はない
(define (compile expr env next)
  (cond
   ([symbol? expr]
    (compile-lookup
     expr env
     (lambda (n m)
       `(:refer ,n ,m ,next))))
   ([pair? expr]
    (record-case expr
     ([quote obj]
      `(:constant ,obj ,next))
     ([lambda vars body]
      `(:close
	,(compile body
		  (extend env vars)
		  `(:return ,(+ (length vars) 1)))
	,next))
     ([if test then els]
      (let ((thenc (compile then next))
	    (elsec (compile els next)))
	(compile test env `(:test ,testc ,elsec))))
     ([set! var x]
      (compile-lookup
       var env
       (lambda (n m)
	 (compile x env `(:assign ,n ,m ,next)))))
     (else
      (let loop ([args (cdr expr)]
		 [c (compile (car expr) env '(:apply))])
	(if (null? args)
	    `(:frame ,next ,c)
	    (loop
	     (cdr args)
	     (compile
	      (car args)
	      env
	      `(:argument ,c))))))))
   (else
    `(:constant ,expr ,next))))

;; 4.1.6 Evaluation

;; registers
;; a: accumulator
;; x: next expression
;; s: stack pointer (top-of-stack)
;; e: stack pointer (call frame of next outer scope)

(define (find-link n env)
  (if (= n 0)
      env
      (find-link (- n 1) (index env -1))))

(define (VM a expr env stack-top)
  (record-case expr
   ([:halt] a)
   ([:refer n m x]
    (VM (index (find-link n env) m) x env stack-top))
   ([:constant obj x]
    (VM obj x env stack-top))
   ([:close body x]
    (VM (functional body env) x env stack-top))
   ([:test then els]
    (VM a (if a then els) env stack-top))
   ([:assign n m x]
    (index-set! (find-link n env) m a))
   ([:frame ret x]
    (VM a x env (push ret (push env stack-top))))
   ([:argument x]
    (VM a x env (push a stack-top)))
   ([:apply]
    (let ((body (list-ref a 0))
	  (link (list-ref a 1)))
      (VM a body stack-top (push link stack-top))))
   ([:return n]
    (let ([s (- stack-top n)])
      (VM a (index s 0) (index s 1) (- s 2))))))

(define (evaluate x)
  (VM '() (compile x '() '(:halt)) 0 0))

