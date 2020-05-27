#lang racket

(define data-definitions
  '((data vars
          (nil)
          (cons (x int) (rest vars)))
    (data expr
          (num (n int))
          (var (y int))
          (abs (xs vars) (e expr))
          (app (e_1 expr) (e_2 expr)))
    (data value
          (num (n int))
          (clos (e expr) (E env))
          (prim (x prim)))
    (data env
          (nil)
          (cons (v value) (rest env)))
    (data frame
          (app1 (e_2 expr) (E env))
          (app2 (v_1 value)))
    (data cont
          (nil)
          (cons (f frame) (rest cont)))
    (data state
          (eval (e expr) (E env) (k cont))
          (cont (v value) (k cont)))))

(define (variant->enum name)
  (λ (variant) (string-upcase (format "~a_~a" name variant))))

(define primitive-types
  '(int prim char str))

(define (data->constructor d)
  (match-let ([`(data ,name . ,body) d])
    (string-join
     (for/list ([case body])
       (match-let ([`(,variant-name . ,fields) case])
         (let* ([type-name name]
                [procedure-name (format "new_~a_~a" name variant-name)]
                [formals (string-join
                          (for/list ([field fields])
                            (match-let ([`(,x ,t) field])
                              (format "~a ~a" t x)))
                          ",")]
                [alloc (string-append
                        (format "~a tmp = malloc(sizeof(struct ~a));" name name)
                        "if (! tmp) { printf(\"Out of memory\\n.\"); exit(-1); }")]
                [init-gc
                 "tmp->next = gc_head; gc_head = (object) tmp; gc_head->useful = true;"]
                [init-tag (format "tmp->kind = ~a;"
                                  ((variant->enum name) variant-name))]
                [init-field (string-join
                       (for/list ([field fields])
                         (match-let ([`(,x ,t) field])
                           (format "tmp->u.~a.~a = ~a;"
                                   variant-name x x))))]
                [return "return tmp;"]
                [body (string-join
                       (list alloc
                             init-gc
                             init-tag
                             init-field
                             return))])
           (format "~a ~a(~a) {~a}"
                   type-name
                   procedure-name
                   formals
                   body))))
     "\n")))

(define (data->struct-definition d)
  (match-let ([`(data ,name . ,body) d])
    (let ([variants (map car body)])
      (string-join
       (list
        (format "typedef struct ~a {" name)
        (format "bool useful;")
        (format "object next;")
        (format "enum {~a} kind;"
                (string-join (map (variant->enum name) variants) ","))
        (format "union {~a} u;"
                (string-join
                 (for/list ([case body])
                   (match-let ([`(,variant-name . ,fields) case])
                     (format "struct {~a} ~a;"
                             (string-join
                              (for/list ([field fields])
                                (match-let ([`(,field-name ,type) field])
                                  (format "~a ~a;"
                                          (if (memv type primitive-types) type
                                              (format "struct ~a *" type))
                                          field-name))))
                             variant-name)))))
        (format "} *~a;" name))
       "\n"))))

(define (data->gc-defs d)
  (match-let ([`(data ,name . ,body) d])
    (format "void gc_mark_~a(~a tmp);" name name)))

(define (data->gc-mark d)
  (match-let ([`(data ,name . ,body) d])
    (let ([variants (map car body)])
      (string-join
       (list
        (format "void gc_mark_~a(~a tmp) {" name name)
        (format "tmp->useful = true;")
        (format "switch (tmp->kind) {")
        (string-join
         (for/list ([variant body])
           (match-let ([`(,variant-name . ,fields) variant])
             (string-join
              (list
               (format "case ~a:" ((variant->enum name) variant-name))
               (string-join
                (for/list ([field fields])
                  (match-let ([`(,field-name ,type) field])
                    (format "gc_mark_~a(tmp->u.~a.~a);"
                            type variant-name field-name))))
               (format "return;"))))))
        (format "}")
        (format "printf(\"Internal error ~a.\\n\");exit(-1);return;}" (random 100)))
       "\n"))))

(define (generate-header name ds)
  (displayln (format "#ifndef ~a_H" name))
  (displayln (format "#define ~a_H" name))
  (displayln (format "#include \"gc.h\""))
  (displayln
   (string-join
    (map data->struct-definition ds)
    "\n\n"))
  (displayln
   (string-join
    (map data->constructor ds)
    "\n\n"))
  (newline)
  (for ([name primitive-types])
    (displayln (format "void gc_mark_~a(~a tmp);" name name)))
  (displayln
   (string-join
    (map data->gc-defs ds)))
  (newline)
  (displayln
   (string-join
    (map data->gc-mark ds)
    "\n\n"))
  (displayln (format "#endif /* ~a_H */" name)))


(generate-header 'SIC data-definitions)
