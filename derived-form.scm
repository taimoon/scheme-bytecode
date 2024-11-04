(load "match.scm")
(load "match-defmacro.scm")
(load "set-collection.scm")
(define (let*->let e)
  (if (null? (cadr e))
      (cons 'begin (cddr e))
      (let*->let-aux (cadr e) (cddr e))))

(define (let*->let-aux bindings body)
  (if (null? (cdr bindings))
      (list* 'let (list (car bindings)) body)
      (list 'let (list (car bindings)) (let*->let-aux (cdr bindings) body))))

(define (let->app e)
#;(let loop ([x v] ...) body)
#;(let ([loop 0]) (set! loop (lambda x . body)) (loop . v))
  (if (symbol? (cadr e))
      (let ([fn (cadr e)]
            [params (map car (caddr e))]
            [init (map cadr (caddr e))]
            [body (cdddr e)])
        (let->app
          (list 'let (list (list fn 0))
            (list 'set! fn (list* 'lambda params body))
            (cons fn init))))
      (cons (list* 'lambda (map car (cadr e)) (cddr e))
        (map cadr (cadr e)))))

(define (case->if e)
  (let ((sym (gensym)))
    (list* 'let (list (list sym (cadr e)))
      (list (case->if-aux sym (cddr e))))))

(define (case->if-aux pred-sym clauses)
  (let ([clause (car clauses)]
        [rest (cdr clauses)])
    (if (eq? (caar rest) 'else)
        (list 'if (list 'member pred-sym (list 'quote (car clause)))
              (cons 'begin (cdr clause))
              (cons 'begin (cdar rest)))
        (list 'if (list 'member pred-sym (list 'quote (car clause)))
              (cons 'begin (cdr clause))
              (if (null? rest) (void) (case->if-aux pred-sym rest))))))

(define (or->ifs es)
  (if (pair? es)
      (list 'if (car es) #t (or->ifs (cdr es)))
      #f))

(define (and->ifs es)
  (if (pair? es)
      (list 'if (car es) (and->ifs (cdr es)) #f)
      #t))

(define (cond->ifs clauses)
  (let ([clause (car clauses)]
        [rest (cdr clauses)])
    (if (eq? (caar rest) 'else)
        (list 'if (car clause)
              (cons 'begin (cdr clause))
              (cons 'begin (cdar rest)))
        (list 'if (car clause)
              (cons 'begin (cdr clause))
              (if (null? (cdr rest)) (list 'void) (cond->ifs rest))))))

#;(`(a b ,c) ===> (list `a `b c))
#;(`(a ,b ,@c) ===> (append (list 'a) (list 'b) c))
#;(`(a . ,c) ===> (cons 'a c))
#;(`(a . ,@c) ===> (list 'a 'unquote-splicing 'c))
(define (expand-quasiquote form)
  (expand-qq (cadr form)))

(define (expand-qq form)
  (cond 
    ((not (pair? form)) (list 'quote form))
    ((eq? 'quasiquote (car form)) (expand-qq (cadr form)))
    ((eq? 'unquote (car form)) (cadr form))
    (else (qq-list form))))

(define (tail-unquote? form)
  (and
    (pair? form)
    (pair? (cdr form))
    (null? (cddr form))
    (eq? (car form) 'unquote)))

(define (tail-unquote a)
  (cadr a))

(define (qq-list form)
  (cond 
    [(null? form) ''()]
    [(not (pair? form))
     (list 'list (expand-qq form))]
    [(tail-unquote? form)
     (tail-unquote form)]
    [(and (pair? (car form))
          (eq? 'unquote-splicing (caar form)))
      (list 'append (cadar form) (qq-list (cdr form)))]
    [else (list 'append (list 'list (expand-qq (car form))) (qq-list (cdr form)))]))

(define (desugar e)
  (match e
    [(quote ,q) `(quote ,q)]
    [(quasiquote ,q) (desugar (expand-qq q))]
    [,v (guard (not (pair? v))) v]
    ;;; workaround
    [(if ,e1 ,e2)
     `(if ,(desugar e1) ,(desugar e2) (void))]
    [(if ,e1 ,e2 ,e3)
     `(if ,(desugar e1)
          ,(desugar e2)
          ,(desugar e3))]
    [(set! ,x ,e)
     (list 'set! x (desugar e))]
    [(begin . ,es)
     (cons 'begin (map desugar es))]
    [(lambda ,params . ,body)
     `(lambda ,params . ,(map desugar body))]
    [(let . ,()) (desugar (let->app e))]
    [(let* . ,()) (desugar (let*->let e))]
    [(case . ,()) (desugar (case->if e))]
    [(cond . ,()) (desugar (cond->ifs (cdr e)))]
    [(or . ,()) (desugar (or->ifs (cdr e)))]
    [(and . ,()) (desugar (and->ifs (cdr e)))]
    [(not . ,()) (desugar (list 'if (cadr e) #f #t))]
    [(define (,fn . ,params) . ,body)
     `(define (,fn . ,params) . ,(cdr (map desugar (cons 'begin body))))]
    [(define ,x ,e) `(define ,x ,(desugar e))]
    [(match . ,()) (desugar (compile-match e))]
    [(define-syntax . ,()) ''()]
    ; the desugar wouldn't desugar cxxr alone
    ; for example, (map cadr bindings)
    [(,op . ,es)
     (map desugar (cons op es))]
    [,e (error desugar 'unknown:e e)]))

(define (quote->cons q)
  (match q
    [(quote (,left . ,right))
     `(cons ,(quote->cons `(quote ,left)) ,(quote->cons `(quote ,right)))]
    [(quote ()) ''()]
    [(quote ,q) (guard (not (symbol? q))) q]
    [(quote ,q) `(quote ,q)]
    [,() (quote->cons 'unknown:quotation q)]))

(define (desugar-op e)
  (match e
    [(quote ,()) (desugar (quote->cons e))]
    [,v (guard (not (pair? v))) v]
    [(if ,e1 ,e2 ,e3)
     `(if ,(desugar-op e1)
          ,(desugar-op e2)
          ,(desugar-op e3))]
    [(set! ,x ,e)
     (list 'set! x (desugar-op e))]
    [(begin . ,es)
     (cons 'begin (map desugar-op es))]
    [(lambda ,params . ,body)
     `(lambda ,params . ,(map desugar-op body))]
    [(define (,fn . ,params) . ,body)
     `(define (,fn . ,params) . ,(cdr (map desugar-op (cons 'begin body))))]
    [(define ,x ,e)
     `(define ,x ,(desugar-op e))]
    [(list) ''()]
    [(list ,e . ,es)
     (list 'cons (desugar-op e) (desugar-op (cons 'list es)))]
    [,e (map desugar-op e)]
    [,() (error desugar-op 'unknown:e e)]))


;;; convert-internal-definition
#|
  ;;; see r7rs
  ;;; 5.3.2. Internal definitions
  Definitions can occur at the beginning of a body 
  (that is, the body of a lambda, let, let*, letrec, letrec*, let-values, let*-values, let-syntax, letrec-syntax, parameterize, guard, or case-lambda)
|#
(define (convert-definition-top e)
  (if (and (pair? e) (eq? (car e) 'begin))
      (cons 'begin
            (map (lambda (e) (convert-definition e #t))
                 (cdr e)))
      (convert-definition e #f)))

(define (collect-definition e)
  (match e
    [,() (guard (not (pair? e))) (make-set)]
    [(quote ,e) (make-set)]
    [(define-syntax . ,()) (make-set)]
    [(define (,fn . ,()) . ,()) (make-set fn)]
    [(define ,x . ,()) (make-set x)]
    [(if ,pred ,conseq ,alter) (make-set)]
    [(begin . ,es)
     (fold-left set-union (make-set) (map collect-definition es))]
    [(set! ,x ,e) (make-set)]
    [(lambda ,params . ,body) (make-set)]
    [(load ,e) (make-set)]
    [,es (make-set)]))

(define (convert-definition e ctx)
  (match e
    [,() (guard (not (pair? e))) e]
    [(quote ,()) e]
    [(define-syntax . ,()) e]
    [(define (,fn . ,params) . ,body)
      (if ctx
          `(set! ,fn ,(convert-definition `(lambda ,params . ,body) #t))
          (error convert-definition 'invalid-context e))]
    [(define ,x ,e*)
     (if ctx
         `(set! ,x ,(convert-definition e* #f))
         (error convert-definition 'invalid-context e))]
    [(if ,pred ,conseq ,alter)
     `(if ,(convert-definition pred #f) 
          ,(convert-definition conseq #f) 
          ,(convert-definition alter #f))]
    [(begin . ,es)
     (if ctx
         (let ([vars (collect-definition e)])
          (if (null? vars)
              (cons 'begin (map (lambda (e) (convert-definition e #t)) es))
              (cons (list* 'lambda vars (map (lambda (e) (convert-definition e #t)) es))
                    (map (lambda _ 0) vars))))
         (cons 'begin (map (lambda (e) (convert-definition e #f)) es)))]
    [(set! ,x ,e*) `(set! ,x ,(convert-definition e* #f))]
    [(lambda ,params . ,body) 
      (let ([e* (convert-definition (cons 'begin body) #t)])
        (if (eq? (car e*) 'begin)
            (list* 'lambda params (cdr e*)))
            (list* 'lambda params (list e*)))]
    [(load ,f) `(load ,(convert-definition f #f))]
    [,es (map (lambda (e) (convert-definition e #f)) es)]))

;;; free variables
(define (improper->proper xs)
  (if (pair? xs)
      (cons (car xs) (improper->proper (cdr xs)))
      (if (null? xs)
          '()
          (list xs))))

(define (lambda-free-vars e)
  (set-diff 
    (fold-left set-union (make-set) (map free-vars (cddr e))) 
    (improper->proper (cadr e))))

(define (free-vars e)
  (match e
    [,() (guard (symbol? e)) (make-set e)]
    [,() (guard (or (number? e) (boolean? e) (char? e) (string? e))) (make-set)]
    [(quote ,e) (make-set)]
    [(if ,pred ,conseq ,alter)
      (fold-left set-union (make-set) (map free-vars (list pred conseq alter)))]
    [(begin . ,es) (fold-left set-union (make-set) (map free-vars es))]
    [(set! ,x ,e) (set-union (free-vars e) (make-set x))]
    [(lambda ,() . ,()) (lambda-free-vars e)]
    [(load ,file) (free-vars file)]
    [,es (fold-left set-union (make-set) (map free-vars es))]))

;;; trim procedure
(define (trim-proc e)
  (match e
    [,() (guard (not (pair? e))) e]
    [(quote ,q) `',q]
    [(if ,pred ,conseq ,alter)
     `(if ,(trim-proc pred) 
          ,(trim-proc conseq) 
          ,(trim-proc alter))]
    [(begin . ,es) (cons 'begin (map trim-proc es))]
    [(set! ,x ,e) `(set! ,x ,(trim-proc e))]
    [(lambda ,params . ,body)
     `(lambda* ,(lambda-free-vars e) ,params . ,(map trim-proc body))]
    [(load ,file) (list 'load (trim-proc file))]
    [,es (map trim-proc es)]))

;;; translate
(define (make-senv xs)
  (list
    (map (lambda (bs) (list (car bs) 0 (cadr bs)))
         (precompute-rank xs))))
(define (precompute-rank xs)
  (let recur ([xs xs] [i 0])
    (if (pair? xs)
        (cons (list (car xs) i) (recur (cdr xs) (+ i 1)))
        (if (symbol? xs)
            (list (list xs i))
            '()))))

(define (extend-senv xs env)
  (cons
    (map (let ([l (length env)])
          (lambda (bs) (list (car bs) l (cadr bs))))
         (precompute-rank xs))
    env))

(define (append-to-curr-senv! xs senv)
  (let ([l (length senv)]
        [r (length (car senv))])
    (set-car!
      senv
      (append (car senv)
              (map (lambda (bs) (list (car bs) 0 (+ r (cadr bs))))
                   (precompute-rank xs))))))

(define (append-to-curr-global! xs senv)
  (let ([l (length senv)]
        [r (length (car senv))])
    (set-car!
      senv
      (append (car senv)
              (map (lambda (bs) (list (car bs) 'global (+ r (cadr bs))))
                   (precompute-rank xs))))))

(define (assoc-senv x senv)
  (let loop ([senv senv])
    (if (null? senv)
      #f
      (let ([res (assoc x (car senv))])
        (if res
            (cdr res)
            (loop (cdr senv)))))))

(define (apply-senv x env)
  (let ([res (assoc-senv x env)])
    (if res
        res
        (error apply-senv 'apply-senv:unbound:x x env))))

(define (translate e senv)
  (match e
    [,() (guard (symbol? e)) (cons 'svar (apply-senv e senv))]
    [,() (guard (or (number? e) (vector? e) (boolean? e) (char? e) (string? e))) e]
    [(quote ,e) `',e]
    [(if ,pred ,conseq ,alter)
      `(if ,(translate pred senv)
           ,(translate conseq senv)
           ,(translate alter senv))]
    [(begin . ,es)
     (cons 'begin
      (let loop ([es es])
        (if (null? es)
            '()
            (cons (translate (car es) senv) (loop (cdr es))))))]
    [(set! ,x ,e)
     (let ([x (apply-senv x senv)]
           [e (translate e senv)])
      `(set! ,x ,e))]
    [(lambda ,params . ,body)
     `(lambda ,params . ,(cdr (translate (cons 'begin body) (extend-senv params senv))))]
    [(lambda* ,free-vars ,params . ,body)
     `(lambda* 
        ,(map (lambda (x) (apply-senv x senv)) free-vars) 
        ,params
        . ,(cdr (translate (cons 'begin body) (make-senv (append free-vars params)))))]
    [(load ,file) `(load ,(translate file senv))]
    [(,op . ,es)
     `(,(translate op senv) . ,(map (lambda (e) (translate e senv)) es))]
    ))
