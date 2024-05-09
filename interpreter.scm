(load "match.scm")
(load "sexp-file-io.scm")
(load "derived-form.scm")
(load "set-collection.scm")
(load "mutables.scm")

;;; ribcage representation of environment
(define (bindings->boxes ps)
  (map (lambda (p) (list (car p) (make-box (cadr p)))) ps))

(define (make-env ps) (list (bindings->boxes ps)))

(define (assoc-env x env)
  (if (null? env)
      #f
      (let ([res (assoc x (car env))])
        (if res
            res
            (assoc-env x (cdr env))))))

(define (apply-env x env)
  (let ([res (assoc-env x env)])
    (if (pair? res)
        (box-val (cadr res))
        (error apply-env 'unbound:x x (map (lambda (e) (map car e)) env)))))

(define (zip-2 xs vs)
  (cond
    [(symbol? xs) (list (list xs vs))]
    [(pair? xs)
     (if (pair? vs)
         (cons (list (car xs) (car vs))
               (zip-2 (cdr xs) (cdr vs)))
         (error zip-2  'too-few-values))]
    [(null? vs)
     (if (null? xs)
         '()
         (error zip-2 'too-many-values))]
    [else (error zip-2 'unknown (cons xs vs))]))

(define (extend-env/binding bs env)
  (cons (bindings->boxes bs) env))

(define (extend-env xs vs env)
  (extend-env/binding (zip-2 xs vs) env))

(define (append-to-curr-env! bs env)
  (set-car! env (append (car env) (bindings->boxes bs))))

(define (update-env! x v env)
  (let ([res (assoc-env x env)])
    (if (pair? res)
        (box-set! (cadr res) v)
        (error update-env! 'unbound:x x))))

(define (evlis exps env)
  (if (pair? exps)
      (cons (interp (car exps) env) (evlis (cdr exps) env))
      '()))

(define (eprogn exps env)
  ;;; Other dialect use (progn ...) instead of (begin ...)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (interp (car exps) env) (eprogn (cdr exps) env))
          (interp (car exps) env))
      (void)))

(define (interp e env)
  (match e
    [,() (guard (or (number? e) (boolean? e) (char? e) (string? e))) e]
    [,() (guard (symbol? e)) (apply-env e env)]
    [(quote ,e) e]
    [(if ,pred ,conseq ,alter)
      (if (interp pred env)
          (interp conseq env)
          (interp alter env))]
    [(begin . ,es) (eprogn es env)]
    [(set! ,x ,e) (update-env! x (interp e env) env)]
    [(lambda* ,free-vars ,params . ,body)
     (make-trim-function free-vars params (cons 'begin body) env)]
    [(lambda ,params . ,body) (make-function params (cons 'begin body) env)]
    [(load ,file)
     (interp-top (cons 'begin (read-sexps (interp file env))))]
    [(,op . ,es) (invoke (interp op env) (evlis es env))]))

(define (interp-top e)
  (let* 
    ([e* (desugar e)]
     [exports (collect-definition e*)]
     [e* (convert-definition-top e*)]
     [imports (free-vars e*)]
     [syms (append imports exports)]
     [e* (trim-proc e*)]
     [_ (GLOBAL.VALS.EXTEND (zip-2 syms syms))]
     )
    (interp e* GLOBAL.VALS)
    ))

(define (interp* e)
  (GLOBAL.VALS.INIT)
  (GLOBAL.VALS.EXTEND (primitives.impl))
  (interp-top e))

(define (invoke proc args)
  (if (procedure? proc)
      (apply proc args)
      (error invoke 'expect-a-proc proc)))

(define (make-function params body env)
  (lambda values
    (interp body (extend-env params values env))))

(define (make-trim-function free-vars params body env)
  (let (
    [bs (map (lambda (x)
                (let ([res (assoc-env x env)])
                  (if res
                      res
                      (error make-trim-function 'unbound:x x))))
             free-vars)])
  (lambda values
    (interp body (list (append bs (bindings->boxes (zip-2 params values))))))))

;;; globals
(define GLOBAL.VALS (make-env '()))

(define (GLOBAL.VALS.INIT)
  (set! GLOBAL.VALS (make-env '())))

(define (GLOBAL.VALS.EXTEND pairs)
  (let ([syms (set-diff (map car pairs)
                        (map car (car GLOBAL.VALS)))])
    (append-to-curr-env!
      (filter (lambda (p) (member (car p) syms)) pairs) GLOBAL.VALS)))

(define (primitives.impl)
  (list
  (list 'apply apply)
  (list '+ +)
  (list '- -)
  (list '* *)
  (list 'div div)
  (list '<= <=)
  (list '= =)
  (list '< <)
  ;;; predicate
  (list 'equal? equal?)
  (list 'eq? eq?)
  (list 'char? char?)
  (list 'vector? vector?)
  (list 'number? number?)
  (list 'boolean? boolean?)
  (list 'string? string?)
  (list 'pair? pair?)
  (list 'null? null?)
  (list 'symbol? symbol?)
  (list 'procedure? procedure?)
  ;;; others
  (list 'void void)
  (list 'gensym gensym)
  (list 'read read)
  (list 'cons cons)
  (list 'car car)
  (list 'cdr cdr)
  (list 'set-car! set-car!)
  (list 'set-cdr! set-cdr!)
  ;;; script
  (list 'command-line command-line)
  (list 'error error)
  ;;; file-io
  (list 'read-sexps read-sexps)
  (list 'pretty-print pretty-print)
  (list 'display display)
  (list 'newline newline)
  (list 'call-with-port call-with-port)
  (list 'eof-object? eof-object?)
  (list 'open-input-file open-input-file)
  (list 'open-output-file open-output-file)
  (list 'open-file-output-port open-file-output-port)
  (list 'open-file-input-port open-file-input-port)
  ;;; derivable
  (list 'list list)
  (list 'list* list*)
  (list 'reverse reverse)
  (list 'append append)
  (list 'member member)
  (list 'assoc assoc)
  (list 'map map)
  (list 'filter filter)
  (list 'fold-left fold-left)
  (list 'caar caar)
  (list 'cdar cdar)
  (list 'cadr cadr)
  (list 'cddr cddr)
  (list 'caaar caaar)
  (list 'cadar cadar)
  (list 'caadr caadr)
  (list 'caddr caddr)
  (list 'cdaar cdaar)
  (list 'cddar cddar)
  (list 'cdadr cdadr)
  (list 'cdddr cdddr)
  (list 'caaaar caaaar)
  (list 'caadar caadar)
  (list 'caaadr caaadr)
  (list 'caaddr caaddr)
  (list 'cadaar cadaar)
  (list 'caddar caddar)
  (list 'cadadr cadadr)
  (list 'cadddr cadddr)
  (list 'cdaaar cdaaar)
  (list 'cdadar cdadar)
  (list 'cdaadr cdaadr)
  (list 'cdaddr cdaddr)
  (list 'cddaar cddaar)
  (list 'cdddar cdddar)
  (list 'cddadr cddadr)
  (list 'cddddr cddddr)
))