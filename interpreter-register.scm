; (load "match.scm")
; (load "sexp-file-io.scm")
; (load "derived-form.scm")
; (load "set-collection.scm")
; (load "mutables.scm")
(load "interpreter-meaning.scm")
(define PC '())
(define PROG (list->vector '()))

;;; linearize
(define (linearize e)
  (match e
    [(CONSTANT ,v) `((CONSTANT ,v))]
    [(ALTERNATIVE ,pred ,conseq ,alter)
      (let ([conseq-lbl (gensym "conseq")]
            [alter-lbl (gensym "alter")]
            [end-lbl (gensym "alter-end")])
      `(,@(linearize pred)
        (IF ,conseq-lbl)
        ,@(linearize alter)
        (GOTO ,end-lbl)
        (LABEL ,conseq-lbl)
        ,@(linearize conseq)
        (LABEL ,end-lbl)))]
    [(DEEP-FETCH global ,rank)
     `((GLOBAL-FETCH ,rank))]
    [(DEEP-SET! global ,rank ,m)
     `(,@(linearize m)
       (GLOBAL-SET! ,rank))]
    [(DEEP-FETCH 0 ,rank)
     `((LOCAL-FETCH ,rank))]
    [(DEEP-SET! 0 ,rank ,m)
     `(,@(linearize m)
       (LOCAL-SET! ,rank))]
    [(SEQUENCE ,m ,m*)
     (append (linearize m) (linearize m*))]
    [(TAIL-CALL ,m ,m*)
     (let ([end (gensym "call-end")])
      `(,@(linearize m)
        (PUSH-VAL)
        ,@(linearize m*)
        (POP-PROC)
        (TAIL-CALL-PROC)))]
    [(CALL ,m ,m*)
     (let ([end (gensym "call-end")])
      `((PUSH-ENV)
        ,@(linearize m)
        (PUSH-VAL)
        ,@(linearize m*)
        (POP-PROC)
        (CALL-PROC)
        (POP-ENV)))]
    [(STORE-ARG ,m ,m*)
     `(,@(linearize m)
       (PUSH-VAL)
       ,@(linearize m*)
       (POP-PROC)
       (CONS-PROC-VAL))]
    [(INIT-EMPTY-LIST)
     `((NULL))]
    [(MAKE-FUNCTION ,free-vars ,arity ,body)
     (let ([ptr (gensym "function")]
           [end (gensym "function-end")])
      `((GOTO ,end)
        (LABEL ,ptr)
        ,@(linearize body)
        (RETURN)
        (LABEL ,end)
        (FUNCTION ,ptr ,free-vars ,arity)))]
    [(LOAD ,m)
     `(,@(linearize m)
       (LOAD))]
    [,() (error linearize 'unknown e)]))

(define (label->addr lbl corspd)
  (list (cadr (assoc lbl corspd))))

(define (replace-label prog corspd)
  (define (lbl->nums lbl) (label->addr lbl corspd))
  (define (replace-line ln)
    (match ln
      [(IF ,lbl)
       `(IF . ,(lbl->nums lbl))]
      [(LABEL ,()) '()]
      [(GOTO ,lbl) `(GOTO . ,(lbl->nums lbl))]
      [(FUNCTION ,ptr . ,args)
       `(FUNCTION ,@(lbl->nums ptr) . ,args)]
      [,()
        (guard (member (car ln) 
          '(PUSH-ENV PUSH-PROC PUSH-VAL POP-PROC POP-ENV
            CONSTANT NULL TRUE FALSE NEGINT INT STR SYM CHAR
            LOCAL-FETCH LOCAL-SET! GLOBAL-FETCH GLOBAL-SET!
            RETURN TAIL-CALL-PROC CALL-PROC CONS-PROC-VAL NULL
            LOAD FINISH)))
       ln]
      [,() (error replace-label 'unknown ln)]))
  (let recur ([prog prog])
    (if (null? prog)
      '()
      (let ([ln (replace-line (car prog))]
            [rest (recur (cdr prog))])
        (if (null? ln)
            rest
            (cons ln rest))))))

(define (instr-length . _) 1)

(define (extract-label i prog corspd)
  (match prog
    [() corspd]
    [((LABEL ,lbl) . ,prog)
     (extract-label i prog (cons (list lbl i) corspd))]
    [(,ln . ,prog) (extract-label (+ i (instr-length ln)) prog corspd)]
    [,prog (error extract-label 'unmatch:prog prog)]))

(define (replace-label-top prog)
  (replace-label prog (extract-label 0 prog '())))

(define sexp-relocate-identity
  (lambda (ln offset env) ln))
;;; instr-interp-tbl
(define instr-interp-tbl (create-table))
(begin
  (table-add! instr-interp-tbl 'CONSTANT 'sexp-interp
    (lambda (i)
      (lambda (ln) (set! VAL (cadr ln)) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'CONSTANT 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'POP-ENV 'sexp-interp
    (lambda (i)
      (lambda (ln) (set! ENV (stack-pop!)) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'POP-ENV 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'POP-VAL 'sexp-interp
    (lambda (i)
      (lambda (ln) (set! VAL (stack-pop!)) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'POP-VAL 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'POP-PROC 'sexp-interp
    (lambda (i)
      (lambda (ln) (set! PROC (stack-pop!)) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'POP-PROC 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'PUSH-ENV 'sexp-interp
    (lambda (i)
      (lambda (ln) (stack-push! ENV) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'PUSH-ENV 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'PUSH-VAL 'sexp-interp
    (lambda (i)
      (lambda (ln) (stack-push! VAL) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'PUSH-VAL 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'PUSH-PROC 'sexp-interp
    (lambda (i)
      (lambda (ln) (stack-push! PROC) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'PUSH-PROC 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'LOCAL-FETCH 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! VAL (box-val (frame-ref ENV (cadr ln))))
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'LOCAL-FETCH 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'LOCAL-SET! 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (box-set! (frame-ref ENV (cadr ln)) VAL)
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'LOCAL-SET! 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'GLOBAL-FETCH 'sexp-interp
    (table-lookup instr-interp-tbl 'LOCAL-FETCH 'sexp-interp))
  (table-add! instr-interp-tbl 'GLOBAL-FETCH 'sexp-relocate
    (lambda (ln offset env)
      (let ([r (cadr (assoc (cadr ln) env))])
        `(LOCAL-FETCH ,r))))
  (table-add! instr-interp-tbl 'GLOBAL-SET! 'sexp-interp
    (table-lookup instr-interp-tbl 'LOCAL-SET! 'sexp-interp))
  (table-add! instr-interp-tbl 'GLOBAL-SET! 'sexp-relocate
    (lambda (ln offset env)
      (let ([r (cadr (assoc (cadr ln) env))])
        `(LOCAL-SET! ,r))))
  (table-add! instr-interp-tbl 'RETURN 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! PC (stack-pop!)))))
  (table-add! instr-interp-tbl 'RETURN 'sexp-relocate sexp-relocate-identity)
  (table-add! instr-interp-tbl 'IF 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (if VAL
            (set! PC (cadr ln))
            (set! PC (+ i PC))))))
  (table-add! instr-interp-tbl 'IF 'sexp-relocate
    (lambda (ln offset env)
      `(IF ,(+ (cadr ln) offset))))
  (table-add! instr-interp-tbl 'GOTO 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! PC (cadr ln)))))
  (table-add! instr-interp-tbl 'GOTO 'sexp-relocate
    (lambda (ln offset env)
      `(GOTO ,(+ (cadr ln) offset))))
  (table-add! instr-interp-tbl 'FUNCTION 'sexp-interp
    (lambda (i)
      (lambda (ln)
        
        (match ln
          [(FUNCTION ,ptr ,free-vars ,arity)
          (set! VAL
              (let ([bs (map (lambda (r) (frame-ref ENV r)) (map cadr free-vars))])
                (lambda _
                  (set! ENV (make-frame (append bs (map make-box (zip-by-arity VAL arity)))))
                  (set! PC ptr)
                  (interp-code))))
          (set! PC (+ i PC))]
          [,() (error 'instr-interp-tbl 'unknown ln)]))))
  (table-add! instr-interp-tbl 'FUNCTION 'sexp-relocate
    (lambda (ln offset env)
      (match ln
        [(FUNCTION ,ptr ,free-vars ,arity)
         `(FUNCTION 
            ,(+ ptr offset)
            ,(map (lambda (bs)
                      (if (eq? (car bs) 'global)
                          (list 0 (cadr (assoc (cadr bs) env)))
                          bs))
                   free-vars)
            ,arity)]
        [,() (error 'instr-interp-tbl 'unknown:sexp-relocate ln)])))
  (table-add! instr-interp-tbl 'TAIL-CALL-PROC 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (PROC))))
  (table-add! instr-interp-tbl 'TAIL-CALL-PROC 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'CALL-PROC 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (stack-push! (add1 PC))
        (PROC))))
  (table-add! instr-interp-tbl 'CALL-PROC 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'CONS-PROC-VAL 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! VAL (cons PROC VAL))
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'CONS-PROC-VAL 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'NULL 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! VAL '())
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'NULL 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'LOAD 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (stack-push! (+ i PC))
        (interp-top (cons 'begin (read-sexps VAL)))
        )))
  (table-add! instr-interp-tbl 'LOAD 'sexp-relocate
    sexp-relocate-identity)
  (table-add! instr-interp-tbl 'FINISH 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! PC (stack-pop!)))))
  (table-add! instr-interp-tbl 'FINISH 'sexp-relocate
    sexp-relocate-identity)
)
(define (load-constant-lib)
  (map
    (lambda (s) (table-add! instr-interp-tbl s 'sexp-relocate sexp-relocate-identity))
    '(TRUE FALSE NEGINT INT STR SYM CHAR))
  (table-add! instr-interp-tbl 'TRUE 'sexp-interp
    (lambda (i) (lambda (ln) (set! VAL #t) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'FALSE 'sexp-interp
    (lambda (i) (lambda (ln) (set! VAL #f) (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'NEGINT 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! VAL (- (byte-list->number (cdr ln))))
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'INT 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (set! VAL (byte-list->number (cdr ln)))
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'STR 'sexp-interp
    (lambda (i)
      (lambda (ln)
        ;;; TODO: workaround
        (if (not (string? (cadr ln)))
            (set! VAL (list->string (map integer->char (cddr ln))))
            (set! VAL (cadr ln)))
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'SYM 'sexp-interp
    (lambda (i)
      (lambda (ln)
        ;;; TODO: workaround
        (if (not (symbol? (cadr ln)))
            (set! VAL (string->symbol (list->string (map integer->char (cddr ln)))))
            (set! VAL (cadr ln)))
        (set! PC (+ i PC)))))
  (table-add! instr-interp-tbl 'CHAR 'sexp-interp
    (lambda (i)
      (lambda (ln)
        (if (not (char? (cadr ln)))
            (set! VAL (integer->char (cadr ln)))
            (set! VAL (cadr ln)))
        (set! PC (+ i PC)))))
)

(load-constant-lib)

(define (interp-code)
  (define tbl
    (map (lambda (b) (list (car b) ((cadr b) 1)))
         (cdr (table-row-fetch instr-interp-tbl 'sexp-interp))))
  (define (find-instr i)
    (let ([f (assoc i tbl)])
      (if f
          (cadr f)
          (error interp-code 'unbound i))))
  (let loop ()
    (if (< PC 0)
        'done
        (let* ([ln (vector-ref PROG PC)]
               [f (find-instr (car ln))])
          (f ln)
          (loop)))))

(define (relocate-sexp IR offset env)
  (define tbl (cdr (table-row-fetch instr-interp-tbl 'sexp-relocate)))
  (define (find-instr i)
    (let ([f (assoc i tbl)])
      (if f
          (cadr f)
          (error relocate-sexp 'unbound i))))
  (map
    (lambda (ln)
      (let ([f (find-instr (car ln))])
        (f ln offset env)))
    IR))

(define (relocate-prog! prog offset env)
  (define end (vector-length prog))
  (define tbl (cdr (table-row-fetch instr-interp-tbl 'sexp-relocate)))
  (define (find-instr i)
    (let ([f (assoc i tbl)])
      (if f
          (cadr f)
          (error relocate-sexp 'unbound i))))
  (let loop ([pc offset])
    (if (< pc end)
        (let* (
          [ln (vector-ref prog pc)]
          [f (find-instr (car ln))])
          (vector-set! prog pc (f ln offset env))
          (loop (add1 pc)))
        'done)))

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
     [IR (linearize m)]
     [IR (replace-label-top IR)]
     [IR (append IR '((FINISH)))]
     [offset (vector-length PROG)]
     )
    (set! ENV GLOBAL.FRAME)
    #;(let* ([relocated (relocate-sexp IR offset id-env)]
           [relocated (list->vector relocated)])
      (set! PROG (vector-append PROG relocated)))
    (set! PROG (vector-append PROG (list->vector IR)))
    (relocate-prog! PROG offset id-env)
    (set! PC offset) (interp-code)
    VAL
  ))

(define (interp* e)
  (GLOBAL.VALS.INIT)
  (GLOBAL.VALS.EXTEND
    (map (lambda (ps) (list (car ps) (convert-primitive (cadr ps))))
         (primitives.impl)))
  (stack-push! -1)
  (set! PROG (list->vector '()))
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
    (set! PC (stack-pop!))
    ))

(define (primtive.apply . _)
  (let ([proc (car VAL)])
   (set! VAL (cadr VAL))
   ;;; TODO: WHY???
   (stack-push! -1)
   (apply proc VAL)
   VAL))


(define (primitive-call-with-port . _)
  (let ([port (car VAL)]
        [proc (cadr VAL)])
    (set! VAL (list port))
    ;;; TODO: WHY???
    (stack-push! -1)
    (proc)
    VAL))