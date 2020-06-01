#lang racket
(require "./compile-core.rkt")

(define (expand-string s)
  (foldr (λ (a d)
           `((cons ,a) ,d))
         'null
         (string->list s)))

(define (expand-data d)
  (cond
    [(null? d) 'null]
    [(integer? d) `(num ,d)]
    [(char? d) `(num ,(char->integer d))]
    [(boolean? d) `(num ,(if d 1 0))]
    [(or (symbol? d) (string? d))
     `(inj-string ,(expand-string (format "~a" d)))]))

(define (expand e)
  (match e
    [`,x #:when (symbol? x) x]
    [`,n #:when (integer? n) n]
    [`,b #:when (boolean? b) (if b 1 0)]
    [`,c #:when (char? c) (char->integer c)]
    [`,s #:when (string? s) (expand (expand-string s))]
    [`(quote ,d) (expand (expand-data d))]
    [`(let () ,e_body)
     (expand e_body)]
    [`(let ([,x ,e_init] . ,rest) ,e_body)
     #:when (symbol? x)
     (expand `((λ (,x) (let ,rest ,e_body)) ,e_init))]
    [`(let ([(fun ,f . ,xs) ,e_init] . ,rest) ,e_body)
     #:when (and (symbol? f) (andmap symbol? xs))
     (expand `((λ (,f) (let ,rest ,e_body)) (λ ,xs ,e_init)))]
    [`(let ([(rec ,f . ,xs) ,e_init] . ,rest) ,e_body)
     #:when (and (symbol? f) (andmap symbol? xs))
     (expand `((λ (,f) (let ,rest ,e_body)) (Z (λ (,f) (λ ,xs ,e_init)))))]
    [`(let ([,x ,e_init] . ,rest) ,e_body)
     #:when (symbol? x)
     (expand `((λ (,x) (let ,rest ,e_body)) ,e_init))]
    [`(if ,e_test ,e_T ,e_F)
     `((((cnd ,(expand e_test))
         (λ (_) ,(expand e_T)))
        (λ (_) ,(expand e_F)))
       _)]
    [`(cond [else ,conseq]) (expand conseq)]
    [`(cond [,test ,conseq] . ,rest)
     (expand `(if ,test ,conseq (cond . ,rest)))]
    [`(and ,e_1 ,e_2)
     (expand `(if ,e_1 ,e_2 #f))]
    [`(and ,e_1 ,e_2 . ,rest)
     (expand `(and (and ,e_1 ,e_2) . ,rest))]
    [`(or ,e_1 ,e_2)
     (expand `(if ,e_1 #t ,e_2))]
    [`(or ,e_1 ,e_2 . ,rest)
     (expand `(or (or ,e_1 ,e_2) . ,rest))]
    [`(λ (,x) ,e)
     `(λ (,x) ,(expand e))]
    [`(λ (,x . ,rest) ,e)
     `(λ (,x) ,(expand `(λ ,rest ,e)))]
    [`(eof-object? ,e)
     `((= EOF) ,(expand e))]
    [`(char->integer ,e) (expand e)]
    [`(integer->char ,e) (expand e)]
    [`(,e_rator ,e_rand)
     `(,(expand e_rator) ,(expand e_rand))]
    [`(,e_rator ,e_rand . ,rest)
     (expand `((,e_rator ,e_rand) . ,rest))]))


(define (stdlib e)
  `(let ([_ #f]
         [(fun zero? n) (= n 0)]
         [(fun sub1 n) (- n 1)]
         [Z (λ (f) ((λ (x) (f (λ (v) ((x x) v)))) (λ (x) (f (λ (v) ((x x) v))))))]
         [mkPair (λ (x y) (λ (f) (f x y)))]
         [fst (λ (pr) (pr (λ (x y) x)))]
         [snd (λ (pr) (pr (λ (x y) y)))]
         [NULL 0]
         [PAIR 1]
         [NUMBER 2]
         [STRING 3]
         [null? (λ (a) (= (fst a) NULL))]
         [pair? (λ (a) (= (fst a) PAIR))]
         [number? (λ (a) (= (fst a) NUMBER))]
         [string? (λ (a) (= (fst a) STRING))]
         [null (mkPair NULL 0)]
         [cons (λ (a d) (mkPair PAIR (mkPair a d)))]
         [car (λ (pr) (fst (snd pr)))]
         [cdr (λ (pr) (snd (snd pr)))]
         [inj-number (λ (n) (mkPair NUMBER n))]
         [inj-string (λ (s) (mkPair STRING s))]
         [unpack (λ (a) (snd a))]
         [(fun <= a b) (or (< a b) (= a b))]
         [(fun >= a b) (or (> a b) (= a b))]
         [(rec append xs ys)
          (cond
            [(null? xs) ys]
            [else (cons (car xs) (append (cdr xs) ys))])]
         [(rec reverse xs)
          (cond
            [(null? xs) null]
            [else (append (reverse (cdr xs))
                          (cons (car xs) null))])]
         [(rec write-string s)
          (if (null? s)
              #f
              (let ([_ (write-char (car s))])
                (write-string (cdr s))))]
         [(fun string->integer s)
          (let ([(rec f s)
                 (cond
                   [(null? s) 0]
                   [else (+ (- (car s) #\0)
                            (* (f (cdr s)) 10))])])
            (f (reverse s)))]
         [(rec integer->string n)
          (let ([(rec f n)
                 (cond
                   [(zero? n) null]
                   [else (cons (+ #\0 (remainder n 10)) (f (quotient n 10)))])])
            (reverse (f n)))]
         [(rec string=? s1 s2)
          (or (and (null? s1) (null? s2))
              (and (pair? s1) (pair? s2)
                   (= (car s1) (car s2))
                   (string=? (cdr s1) (cdr s2))))]
         [(rec equal? x y)
          (or (and (null? x) (null? y))
              (and (number? x) (number? y) (= (unpack x) (unpack y)))
              (and (string? x) (string? y) (string=? (unpack x) (unpack y)))
              (and (pair? x) (pair? y)
                   (equal? (car x) (car y))
                   (equal? (cdr x) (cdr y))))]
         [(rec read-line _)
          (let ([c (read-char _)])
            (if (or (= c #\newline) (= c EOF))
                null
                (cons c (read-line _))))]
         [(fun read-s-exp _)
          (let ([ID 0]
                [EXPR 1]
                [EXPRS 2]
                [(rec parse mode c k)
                 (cond
                   [(= mode ID)
                    (cond
                      [(or (= c #\() (= c #\)) (= c #\space) (= c #\tab) (= c #\newline))
                       (k c null)]
                      [else
                       (parse ID (read-char _)
                              (λ (c^ id)
                                (k c^ (cons c id))))])]
                   [(= mode EXPR)
                    (cond
                      [(= c #\()
                       (parse EXPRS (read-char #f) k)]
                      [(= c #\space)
                       (parse EXPR (read-char #f) k)]
                      [(= c #\')
                       (parse EXPR (read-char _)
                              (λ (c e)
                                (k c (cons (inj-string "quote") (cons e null)))))]
                      [else
                       (parse ID c (λ (c id) (k c (inj-string id))))])]
                   [(= mode EXPRS)
                    (cond
                      [(= c #\))
                       (k (read-char #f) null)]
                      [(= c #\space)
                       (parse EXPRS (read-char #f) k)]
                      [else
                       (parse EXPR c (λ (c t_1)
                                       (parse EXPRS c
                                              (λ (c t_2)
                                                (k c (cons t_1 t_2))))))])]
                   [else
                    (let ([_ (write-string "unhandled case in parser\n")])
                      (inj-string ""))])])
            (parse EXPR (read-char _) (λ (c t) t)))]
         [(rec s-expr->string e)
          (cond 
            [(null? e) "()"]
            [(number? e) (integer->string e)]
            [(string? e) (unpack e)]
            [(pair? e)
             (append "("
                     (append (s-expr->string (car e))
                             (append " . "
                                     (append (s-expr->string (cdr e)) ")"))))]
            [else
             (let ([_ (write-string "unrecognized S-expr\n")])
               "<error>")])])
     ,e))

(define (compile-user e)
  (compile-core
   (expand
    (stdlib e))))

(define (write-program p)
  (with-output-to-file "./program.h"
    #:exists 'replace
    (λ ()
      (displayln "#include \"sic.h\"")
      (displayln "expr mk_expr() {")
      (displayln (format "return ~a;" p))
      (displayln "}"))))

(define test-program
  #;
  '(write-string "hello world!\n")
  #;
  '(let ([(rec REPL s)
          (let ([_ (write-string "> ")]
                [l (read-line _)]
                [n (string->integer l)]
                [_ (write-string (integer->string n))]
                [_ (write-string "\n")]
                [s (+ n s)]
                [_ (write-string (append (append "sum:" (integer->string s)) "\n"))])
            (REPL s))])
     (REPL 0))
  #;
  '(let ([(rec ! n)
          (if (zero? n)
              1
              (* (! (sub1 n)) n))]
         [(rec REPL _)
          (let ([_ (write-string "> ")]
                [n (string->integer (read-line _))]
                [_ (write-string "factorial(")]
                [_ (write-string (integer->string n))]
                [_ (write-string ")=")]
                [_ (write-string (integer->string (! n)))]
                [_ (write-string "\n")])
            (REPL _))])
     (REPL 0))
  '(let ([R read-s-exp]
         [(fun empty-env _) null]
         [(fun extend-env x v env) (cons (cons x v) env)]
         [(rec lookup x env)
          (cond
            [(null? env)
             (let ([_ (write-string (append "unbound id " (s-expr->string x)))])
               (inj-string "<error>"))]
            [(equal? (car (car env)) x)
             (cdr (car env))]
            [else (lookup x (cdr env))])]
         [(fun E e)
          (let ([(rec valof e env)
                 (let ()
                   (cond
                     [(number? e) e]
                     [(string? e) (lookup e env)]
                     [(null? e)
                      (let ([_ (write-string "invalid expr")])
                        (inj-string "<error>"))]
                     [(equal? (car e) (inj-string "quote"))
                      (let ([d (car (cdr e))])
                        d)]
                     [(equal? (car e) (inj-string "lambda"))
                      (let ([x (car (car (cdr e)))]
                            [body (car (cdr (cdr e)))])
                        (cons (inj-string "clos") (cons (cons x body) env)))]
                     [else
                      (let ([rator (car e)]
                            [rand (car (cdr e))]
                            [f (valof rator env)]
                            [a (valof rand env)]
                            [x (car (car (cdr f)))]
                            [body (cdr (car (cdr f)))]
                            [env (cdr (cdr f))])
                        (valof body (extend-env x a env)))]))])
            (valof e (empty-env _)))]
         [P s-expr->string]
         [(rec L _)
          (let ([_ (write-string "> ")]
                [e (R _)]
                [v (E e)]
                [_ (write-string (P v))]
                [_ (write-string "\n")]
                )
            (L _))])
     (L _)))

(write-program
 (compile-user
  test-program))