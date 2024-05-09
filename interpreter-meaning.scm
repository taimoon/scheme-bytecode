(load "match.scm")
(load "sexp-file-io.scm")
(load "derived-form.scm")
(load "set-collection.scm")
(load "mutables.scm")

(define STACK (make-vector (expt 2 17)))
(define STACK-INDEX 0)
(define (stack-push! obj)
  (vector-set! STACK STACK-INDEX obj)
  (set! STACK-INDEX (add1 STACK-INDEX)))
(define (stack-pop!)
  (set! STACK-INDEX (sub1 STACK-INDEX))
  (vector-ref STACK STACK-INDEX))

(define ENV '())
(define VAL '())
(define PROC '())

;;; meaning
(define (meaning-quotation c tail?)
  `(CONSTANT ,c))

(define (meaning-assignment level rank e tail?)
  `(DEEP-SET! ,level ,rank ,(meaning e #f)))

(define (meaning-reference level rank tail?)
  `(DEEP-FETCH ,level ,rank))

(define (meaning-alternative pred conseq alter tail?)
  (let ([pred (meaning pred #f)]
        [conseq (meaning conseq tail?)]
        [alter (meaning alter tail?)])
    `(ALTERNATIVE ,pred ,conseq ,alter)))

(define (meaning-sequence es tail?)
  (if (pair? es)
      (if (pair? (cdr es))
          (meaning-multi-sequence (car es) (cdr es) tail?)
          (meaning (car es) tail?))
      (error meaning-sequence 'no-empty-sequence es)))

(define (meaning-multi-sequence e es tail?)
  (let* ([m (meaning e #f)]
         [ms (meaning-sequence es tail?)])
    `(SEQUENCE ,m ,ms)))

(define (meaning-trim-abs free-vars params body tail?)
  (let ([body (meaning-sequence body #t)])
    `(MAKE-FUNCTION ,free-vars ,(get-arity params) ,body)))

(define (meaning-load e tail?)
  `(LOAD ,(meaning e tail?)))

(define (meaning-application f-exp es tail?)
  (let* ([m (meaning f-exp #f)]
         [m* (meaning-operands es #f)])
    (if tail?
        `(TAIL-CALL ,m ,m*)
        `(CALL ,m ,m*))))

(define (meaning-operands es tail?)
  (if (pair? es)
      (meaning-some-args (car es) (cdr es) tail?)
      (meaning-no-arg tail?)))

(define (meaning-some-args e es tail?)
  (let* ([m (meaning e tail?)]
         [m* (meaning-operands es tail?)])
    `(STORE-ARG ,m ,m*)))

(define (meaning-no-arg tail?)
  `(INIT-EMPTY-LIST))

(define (meaning e tail?)
  (match e
    [,() (guard (or (number? e) (boolean? e) (char? e) (string? e)))
     (meaning-quotation e tail?)]
    [(svar ,level ,rank) (meaning-reference level rank tail?)]
    [(quote ,e) (meaning-quotation e tail?)]
    [(if ,pred ,conseq ,alter)
     (meaning-alternative pred conseq alter tail?)]
    [(begin . ,es) (meaning-sequence es tail?)]
    [(set! (,level ,rank) ,e) (meaning-assignment level rank e tail?)]
    [(lambda* ,free-vars ,params . ,body) (meaning-trim-abs free-vars params body tail?)]
    [(load ,file) (meaning-load file tail?)]
    [(,op . ,es) (meaning-application op es tail?)]))

;;; helper
(define (get-arity params)
  (let loop ([params params]
             [arity 0])
    (if (pair? params)
        (loop (cdr params) (+ 1 arity))
        (list arity (not (null? params))))))

(define (zip-by-arity vs arity)
  (let ([improper (cadr arity)]
        [k (car arity)])
    (if improper
        (append (list-head vs k) (list (list-tail vs k)))
        vs)))

;;; instruction
(define (CONSTANT v)
  (lambda () (set! VAL v)))

(define (ALTERNATIVE pred conseq alter)
  (lambda ()
    (pred)
    (if VAL
        (conseq)
        (alter))))

(define (LOCAL-SET! rank m)
  (lambda ()
    (m)
    (box-set! (frame-ref ENV rank) VAL)))

(define (LOCAL-FETCH rank)
  (lambda ()
    (set! VAL (box-val (frame-ref ENV rank)))))

(define (SEQUENCE m m+)
  (lambda () (m) (m+)))

(define (TAIL-CALL m m*)
  (lambda ()
    (m)
    (stack-push! VAL)
    (m*)
    (set! PROC (stack-pop!))
    (PROC)))

(define (CALL m m*)
  (lambda ()
    (m)
    (stack-push! VAL)
    (m*)
    (set! PROC (stack-pop!))
    (stack-push! ENV)
    (PROC)
    (set! ENV (stack-pop!))))

(define (STORE-ARG m m*)
  (lambda ()
    (m)
    (stack-push! VAL)
    (m*)
    (set! PROC (stack-pop!))
    (set! VAL (cons PROC VAL))))

(define (INIT-EMPTY-LIST)
  (lambda () (set! VAL '())))

(define (MAKE-FUNCTION free-vars arity body)
  (lambda ()
    (set! VAL (MAKE-CLOSURE ENV free-vars arity body))))

(define (MAKE-CLOSURE env free-vars arity body)
  (let ([bs (map (lambda (r) (frame-ref env r)) (map cadr free-vars))])
    (lambda _
        (set! ENV (make-frame (append bs (map make-box (zip-by-arity VAL arity)))))
        (body))))

(define (LOAD m)
  (lambda ()
    (m)
    (interp-top (cons 'begin (read-sexps VAL)))))

(define (meaning->lambda e)
  (match e
    [(CONSTANT ,v) (CONSTANT v)]
    [(ALTERNATIVE ,pred ,conseq ,alter)
     (ALTERNATIVE
        (meaning->lambda pred)
        (meaning->lambda conseq)
        (meaning->lambda alter))]
    [(DEEP-SET! global ,rank ,m)
     (LOCAL-SET! rank (meaning->lambda m))]
    [(DEEP-FETCH global ,rank)
     (LOCAL-FETCH rank)]
    [(DEEP-SET! 0 ,rank ,m)
     (LOCAL-SET! rank (meaning->lambda m))]
    [(DEEP-FETCH 0 ,rank)
     (LOCAL-FETCH rank)]
    [(SEQUENCE ,m ,m+)
     (SEQUENCE (meaning->lambda m) (meaning->lambda m+))]
    [(TAIL-CALL ,m ,m*)
     (TAIL-CALL (meaning->lambda m) (meaning->lambda m*))]
    [(CALL ,m ,m*)
     (CALL (meaning->lambda m) (meaning->lambda m*))]
    [(STORE-ARG ,m ,m*)
     (STORE-ARG (meaning->lambda m) (meaning->lambda m*))]
    [(INIT-EMPTY-LIST)
     (INIT-EMPTY-LIST)]
    [(MAKE-FUNCTION ,free-vars ,arity ,body)
     (MAKE-FUNCTION free-vars arity (meaning->lambda body))]
    [(LOAD ,m)
     (LOAD (meaning->lambda m))]
    [,()
      (error meaning->lambda 'unknown e)]))

;;; interp
(define (interp-top e)
  (let* 
    ([e* (desugar e)]
     [exports (collect-definition e*)]
     [e* (convert-definition-top e*)]
     [imports (free-vars e*)]
     [syms (append imports exports)]
     [e* (trim-proc e*)]
     [_ (GLOBAL.VALS.EXTEND (map (lambda (s) (list s s)) syms))]
     [id-env (map (lambda (ps) (list (caddr ps) (caddr ps))) (car GLOBAL.SENV))]
     [e* (translate e* GLOBAL.SENV)]
     [m (meaning e* #t)]
     [m* (meaning->lambda m)]
    )
    (set! ENV GLOBAL.FRAME)
    (m*)
    VAL
  ))

(define (interp* e)
  (GLOBAL.VALS.INIT)
  (GLOBAL.VALS.EXTEND
    (map (lambda (ps) (list (car ps) (convert-primitive (cadr ps))))
         (primitives.impl)))
  (interp-top e))

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
    (append-to-curr-global! (map car pairs) GLOBAL.SENV)
    (frame-append! GLOBAL.FRAME (map (lambda (p) (make-box (cadr p))) pairs))))

(define (convert-primitive f)
  (lambda _
    (set! VAL (apply f VAL))
    ))

(define (primtive.apply . _)
  (let ([proc (car VAL)])
   (set! VAL (cadr VAL))
   (apply proc VAL)
   VAL))

(define (primitive-call-with-port . _)
  (let ([port (car VAL)]
        [proc (cadr VAL)])
    (set! VAL (list port))
    (proc)
    VAL))

(define (primitives.impl)
  (list
  (list 'apply primtive.apply)
  (list 'add1 add1)
  (list 'sub1 sub1)
  (list '+ +)
  (list '- -)
  (list '* *)
  (list 'div div)
  (list '<= <=)
  (list '>= >=)
  (list '= =)
  (list '< <)
  (list 'expt expt)
  (list 'ash ash)
  (list 'lognot lognot)
  (list 'logor logor)
  (list 'logand logand)
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
  ;;; symbol,string
  (list 'string->symbol string->symbol)
  (list 'symbol->string symbol->string)
  (list 'string-length string-length)
  (list 'native-transcoder native-transcoder)
  (list 'string->bytevector string->bytevector)
  (list 'bytevector->u8-list bytevector->u8-list)
  (list 'integer->char integer->char)
  (list 'list->string list->string)
  (list 'string-append string-append)
  ;;; vector
  (list 'vector? vector?)
  (list 'make-vector make-vector)
  (list 'list->vector list->vector)
  (list 'vector-ref vector-ref)
  (list 'vector-set! vector-set!)
  (list 'vector-append vector-append)
  (list 'vector-length vector-length)
  ;;; script
  (list 'command-line command-line)
  (list 'error error)
  ;;; file-io
  (list 'pretty-print pretty-print)
  (list 'display display)
  (list 'newline newline)
  (list 'call-with-port primitive-call-with-port)
  (list 'eof-object? eof-object?)
  (list 'open-input-file open-input-file)
  (list 'open-output-file open-output-file)
  (list 'open-file-output-port open-file-output-port)
  (list 'open-file-input-port open-file-input-port)
  (list 'read-sexps read-sexps)
  ;;; derivable
  (list 'list list)
  (list 'append! append!)
  (list 'iota iota)
  (list 'list-ref list-ref)
  (list 'list-head list-head)
  (list 'list-tail list-tail)
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