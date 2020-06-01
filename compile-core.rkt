#lang racket
(provide compile-core)

;; deBruijn->C

(define (deBruijn->C-vars xs)
  (cond
    [(null? xs) "new_vars_nil()"]
    [else
     (format "new_vars_cons(~a,~a)" (car xs)
             (deBruijn->C-vars (cdr xs)))]))

(define (deBruijn->C e)
  (match e
    [`(var ,x) (format "new_expr_var(~a)" x)]
    [`(num ,n) (format "new_expr_num(~a)" n)]
    [`(λ ,xs ,e_b)
     (format "new_expr_abs(~a,~a)"
             (deBruijn->C-vars xs)
             (deBruijn->C e_b))]
    [`(,e_f ,e_a)
     (format "new_expr_app(~a,~a)"
             (deBruijn->C e_f)
             (deBruijn->C e_a))]))

;; lex

(define buildins
  '(EOF + - * quotient remainder < = > cnd read-char write-char))

(define (lex-var x acc)
  (cond
    [(null? acc) (error "unbound" acc x)]
    [else
     (if (eqv? (car acc) x)
         0
         (add1 (lex-var x (cdr acc))))]))

(define (lex-vars acc xs)
  (cond
    [(null? xs) '()]
    [else
     (cons (lex-var (car xs) acc)
           (lex-vars acc (cdr xs)))]))

(define (lex e)
  (let lex ([e e]
            [acc buildins])
    (match e
      [`,x #:when (symbol? x) `(var ,(lex-var x acc))]
      [`,n #:when (number? n) `(num ,n)]
      [`(λ (,x) ,xs ,e)
       `(λ ,(lex-vars acc xs) ,(lex e (cons x xs) ))]
      [`(,e_f ,e_a)
       `(,(lex e_f acc)
         ,(lex e_a acc))])))

;; free-analysis

(define (free-analysis e)
  (car
   (let free-analysis ([e e])
     (match e
       [`,x
        #:when (symbol? x)
        (cons x (set x))]
       [`,n
        #:when (integer? n)
        (cons n (set))]
       [`(λ (,x) ,e)
        (let* ([r (free-analysis e)]
               [xs (set-remove (cdr r) x)])
          (cons `(λ (,x) ,(set->list xs) ,(car r))
                xs))]
       [`(,e_rator ,e_rand)
        (let* ([r_f (free-analysis e_rator)]
               [r_a (free-analysis e_rand)])
          (cons `(,(car r_f) ,(car r_a))
                (set-union (cdr r_f) (cdr r_a))))]))))

(define (compile-core e)
  (deBruijn->C (lex (free-analysis e))))
