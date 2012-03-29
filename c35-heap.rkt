#lang r6rs

;; 3.5.1 Translation and 3.5.2 Evaluation

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

;; aux
(define (tail? next)
  (eq? (car next) ':return))

(define (extend env rib)
  (cons rib env))

(define (compile-lookup var env)
  (let nxtrib ([e env] [rib 0])
    (let nxtelt ([vars (car e)] [elt 0])
      (cond
       ((null? vars) (nxtrib (cdr e) (+ 1 rib)))
       ((eq? (car vars) var)
	(cons rib elt))
       (else (nxtelt (cdr vars) (+ 1 elt)))))))

;; 3.5.1 Translation
(define (compile x env next)
  (cond
   ([symbol? x]
    `(:refer ,(compile-lookup x env) ,next))
   ([pair? x]
    (record-case x
     ([quote obj]
      `(:constant ,obj ,next))
     ([lambda vars body]
      `(:close ,(compile body (extend env vars) '(:return))
	       ,next))
     ([if test then els]
      (let ((thenc (compile then env next))
	    (elsec (compile els env next)))
	(compile test env `(:test ,thenc ,elsec))))
     ([set! var val]
      (let ((access (compile-lookup var env)))
	(compile val env `(:assign ,access ,next))))
     ([call/cc x]
      (let ((c `(:conti
		 `(:argument
		   ,(compile ,x ,env '(:apply))))))
	(if (tail? next)
	    c
	    `(:frame ,next ,c))))
     (else
      (let loop ([args (cdr x)]
		 [c (compile (car x) env '(:apply))])
	(if (null? args)
	    (if (tail? next)
		c
		`(:frame ,next ,c))
	    (loop (cdr args)
		  (compile (car args)
			   env
			   `(:argument ,c))))))))
   (else
    `(:constant ,x ,next))))
	    
;; 3.5.2 Evaluation
;; aux
(define (closure body env)
  `(,body ,env))
(define (continuation stack)
  (closure `(:nuate ,stack '(0 . 0)) '()))
(define (call-frame x e r s)
  `(,x ,e ,r ,s))
;; access = (nth-rib . nth-elt)
(define (lookup access env)
  ;; (let nxtrib ([e env]
  ;; 	       [rib (car access)])
  ;;   (if (= rib 0)
  ;; 	(let nxtelt ([r (car e)]
  ;; 		     [elt (cdr access)])
  ;; 	  (if (= elt 0)
  ;; 	      r
  ;; 	      (nxtelt (cdr r)
  ;; 		      (- elt 1))))
  ;; 	(nxtrib (cdr e) (- rib 1)))))
  (list-ref
   (list-ref env (car access))
   (cdr access)))

(define (replace-var access env val)
  (let ((head-rib (take env (car access) ))
	(tail-rib (drop env (+ 1 (car access))))
	(target-rib (list-ref env (car access))))
    (let ((head-elt (take target-rib (cdr access)))
	  (tail-elt (drop target-rib (+ 1 (cdr access)))))
      `(,@head-rib
	(,@head-elt ,val ,@tail-elt)
	,@tail-rib))))
    
(define (VM a expr env rib stack)
  (record-case expr
   [(:halt) a]
   [(:refer var x)
    (VM (lookup var env) x env rib stack)]
   [(:constant obj x)
    (VM obj x env rib stack)]
   [(:close body x)
    (VM (closure body env) x env rib stack)]
   [(:test then els)
    (VM a (if a then els) env rib stack)]
   [(:assign var x)
    (VM a x (replace-var var env a) rib stack)]
   [(:conti x)
    (VM (continuation stack) x env rib stack)]
   [(:nuate s var)
    (VM (lookup var env) '(:return) env rib s)]
   [(:frame ret x)
    (VM a x env '() (call-frame ret env rib stack))]
   [(:argument x)
    (VM a x env (cons a rib) stack)]
   [(:apply)
    (let ((body (list-ref a 0))
	  (e (list-ref a 1)))
      (VM a body (extend e rib) '() stack))]
   [(:return)
    (let ((x (list-ref stack 0))
	  (e (list-ref stack 1))
	  (r (list-ref stack 2))
	  (s (list-ref stack 3)))
      (VM a x e r s))]))


(define (evaluate x)
  (VM '() (compile x '() '(:halt)) '() '() '()))




