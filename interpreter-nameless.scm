(load "match.scm")
(load "sexp-file-io.scm")
(load "derived-form.scm")
(load "set-collection.scm")
(load "mutables.scm")

(define (zip-like xs vs)
  (cond
    [(symbol? xs) (list vs)]
    [(pair? xs)
     (if (pair? vs)
         (cons (car vs)
               (zip-like (cdr xs) (cdr vs)))
         (error zip-like  'too-few-values))]
    [(null? vs)
     (if (null? xs)
         '()
         (error zip-like 'too-many-values))]
    [else (error zip-like 'unknown (cons xs vs))]))

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
    [(svar 0 ,r) (box-val (frame-ref env r))]
    [(quote ,e) e]
    [(if ,pred ,conseq ,alter)
      (if (interp pred env)
          (interp conseq env)
          (interp alter env))]
    [(begin . ,es) (eprogn es env)]
    [(set! (0 ,r) ,e) (box-set! (frame-ref env r) (interp e env))]
    [(lambda* ,free-vars ,params . ,body)
     (make-trim-function free-vars params (cons 'begin body) env)]
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
     [_ (GLOBAL.VALS.EXTEND (map (lambda (s) (list s s)) syms))]
     [e* (translate e* GLOBAL.SENV)]
     )
    (interp e* GLOBAL.FRAME)
  ))

(define (interp* e)
  (GLOBAL.VALS.INIT)
  (GLOBAL.VALS.EXTEND (primitives.impl))
  (interp-top e))

(define (invoke proc args)
  (if (procedure? proc)
      (apply proc args)
      (error invoke 'expect-a-proc proc)))

(define (make-trim-function free-vars params body env)
  (let ([bs (map (lambda (r) (frame-ref env r)) (map cadr free-vars))])
  (lambda values
    (interp body (make-frame (append bs (map make-box (zip-like params values))))))))

;;; globals
(define GLOBAL.SENV (list '()))
(define GLOBAL.FRAME (make-frame '()))

(define (GLOBAL.VALS.INIT)
  (set! GLOBAL.SENV (list '()))
  (set! GLOBAL.FRAME (make-frame '())))

(define (GLOBAL.VALS.EXTEND pairs)
  (let* ([syms (set-diff (map car pairs)
                         (map car (car GLOBAL.SENV)))]
         [pairs
          (filter (lambda (p) (member (car p) syms)) pairs)])
    (append-to-curr-senv! (map car pairs) GLOBAL.SENV)
    (frame-append! GLOBAL.FRAME (map (lambda (p) (make-box (cadr p))) pairs))))

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
  ;;; vector
  (list 'vector? vector?)
  (list 'list->vector list->vector)
  (list 'vector-ref vector-ref)
  (list 'vector-set! vector-set!)
  (list 'vector-append vector-append)
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
  (list 'length length)
  (list 'list? list?)
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