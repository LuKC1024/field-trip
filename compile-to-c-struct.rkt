#lang racket

(define (translate-vars acc xs)
  (cond
    [(null? xs) "new_vars_nil()"]
    [else (format "new_vars_cons(~a,~a)" (index-of acc (car xs))
                  (translate-vars acc (cdr xs)))]))

(define (free-vars e)
  (match e
    [`,x
     #:when (symbol? x)
     (set x)]
    [`,n
     #:when (integer? n)
     (set)]
    [`,n
     #:when (char? n)
     (set)]
    [`(λ (,x) ,e_body)
     (set-remove (free-vars e_body) x)]
    [`(,e_rator ,e_rand)
     (set-union (free-vars e_rator) (free-vars e_rand))]))

(define (translate e acc)
  (match e
    [`,x
     #:when (symbol? x)
     (unless (index-of acc x)
       (error "unbound" acc x))
     (format "new_expr_var(~a)" (index-of acc x))]
    [`,n
     #:when (integer? n)
     (format "new_expr_num(~a)" n)]
    [`,n
     #:when (char? n)
     (format "new_expr_num(~a)" (char->integer n))]
    [`(λ (,x) ,e)
     (let ([xs (set->list (free-vars `(λ (,x) ,e)))])
       (format "new_expr_abs(~a,~a)"
               (translate-vars acc xs)
               (translate e (cons x xs))))]
    [`(,rator ,rand)
     (format "new_expr_app(~a,~a)"
             (translate rator acc)
             (translate rand acc))]))

(define (compile e)
  (translate e '(EOF + - * quotient remainder = cnd read-char write-char)))

#;
(let ([ifz (λ (x)
             (λ (y)
               (λ (z)
                 (if (zero? x)
                     y
                     z))))]
      [* (λ (m) (λ (n) (* m n)))])
  ((λ (!)
     ((! !) 5))
   (λ (!)
     (λ (n)
       ((((ifz n)
          (λ (_) 1))
         (λ (_)
           ((* ((! !) (- n 1))) n)))
        999)))))
#;
(compile '((λ (!)
             ((! !) 5))
           (λ (!)
             (λ (n)
               ((((ifz n)
                  (λ (_) 1))
                 (λ (_)
                   ((* ((! !) ((- n) 1))) n)))
                999)))))
#;
(compile '((- 5) 3))
#;
(compile '((λ (c) ((λ (_) (writechar c))
                   (writechar c)))
           (readchar 0)))
#;

(let ([sum (λ (sum n)
             (let* ([_ (write-char #\s)]
                    [_ (write-char #\u)]
                    [_ (write-char #\m)]
                    [_ (write-char #\:)]
                    [_ (write-char (integer->char (+ n (char->integer #\0))))]
                    [c (read-char)])
               (if (eof-object? c)
                   0
                   (let* ([m (- (char->integer c) (char->integer #\0))])
                     (sum sum (+ m n))))))])
  (sum sum 0))

(define (complete->core e)
  (match e
    [`,x #:when (symbol? x) x]
    [`,n #:when (integer? n) n]
    [`,c #:when (char? c) c]
    [`(let ([,x ,e_init]) ,e_body)
     `((λ (,x) ,(complete->core e_body))
       ,(complete->core e_init))]
    [`(let* () ,e_body)
     (complete->core e_body)]
    [`(let* ([,x ,e_init] . ,rest) ,e_body)
     (complete->core `(let ([,x ,e_init]) (let* ,rest ,e_body)))]
    [`(if ,e_test ,e_T ,e_F)
     `((((cnd ,(complete->core e_test))
         (λ (_) ,(complete->core e_T)))
        (λ (_) ,(complete->core e_F)))
       777)]
    [`(λ (,x) ,e)
     `(λ (,x) ,(complete->core e))]
    [`(λ (,x . ,rest) ,e)
     `(λ (,x) ,(complete->core `(λ ,rest ,e)))]
    [`(eof-object? ,e)
     `((= EOF) ,(complete->core e))]
    [`(char->integer ,e)
     (complete->core e)]
    [`(integer->char ,e)
     (complete->core e)]
    [`(,e_rator ,e_rand)
     `(,(complete->core e_rator) ,(complete->core e_rand))]
    [`(,e_rator ,e_rand . ,rest)
     (complete->core `((,e_rator ,e_rand) . ,rest))]))
#;
(compile
 (complete->core
  '(let ([sum (λ (sum n)
                (let* ([_ (write-char #\s)]
                       [_ (write-char #\u)]
                       [_ (write-char #\m)]
                       [_ (write-char #\:)]
                       [_ (write-char (integer->char (+ n (char->integer #\0))))]
                       [_ (write-char #\newline)]
                       [c (read-char 0)])
                  (if (eof-object? c)
                      0
                      (let* ([m (- (char->integer c) (char->integer #\0))])
                        (sum sum (+ m n))))))])
     (sum sum 0))))

#;
(compile
 (complete->core
  '(let* ([null (λ (step base) base)]
          [cons (λ (a d)
                  (λ (step base)
                    (step a (d step base))))]
          [foldr (λ (ls base step) (ls step base))])
     (let* ([rec
             (λ (rec n)
               (if (= n 0)
                   null
                   (cons (integer->char (+ (remainder n 2) (char->integer #\0)))
                         (rec rec (quotient n 2)))))]
            [number->string (λ (n)
                              (if (= n 0)
                                  (cons #\0 null)
                                  (rec rec n)))])
       (let ([_ (foldr (number->string 13)
                       0
                       (λ (c _) (write-char c)))])
         (write-char #\newline))))))

(compile
 (complete->core
  '((λ (x) (x x)) (λ (x) (x x)))))

#;
(compile
 (complete->core
  '(let ([! (λ (! n)
              (if (= n 0)
                  1
                  (* (! ! (- n 1)) n)))])
     (! ! 5))))

#;
(let ([cnd (λ (b)
             (λ (x)
               (λ (y)
                 (if (= b 0) y x))))]
      [= (λ (n_1)
           (λ (n_2)
             (if (= n_1 n_2) 1 0)))]
      [- (λ (n_1)
           (λ (n_2)
             (- n_1 n_2)))]
      [* (λ (n_1)
           (λ (n_2)
             (* n_1 n_2)))])
  ((λ (!) ((! !) 5)) (λ (!) (λ (n) ((((cnd ((= n) 0)) (λ (_) 1)) (λ (_) ((* ((! !) ((- n) 1))) n))) 0)))))