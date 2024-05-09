(load "interpreter-register.scm")
(load "marshalling.scm")

(define INT-SZ 4)
(define JMP-SZ 6)

;;; helper
(define (generate-filename prefix affix)
  (string-append prefix (symbol->string (gensym)) affix))


(define (vector-slice v i j)
  (let loop ([i i])
    (if (>= i j)
        '()
        (cons (vector-ref v i) (loop (+ i 1))))))

(define (list-slice xs i j)
  (list-head (list-tail xs i) (- j i)))

(define (vector-list-set! v l i)
  (let loop ([i i] [l l])
    (if (null? l)
        l
        (begin
          (vector-set! v i (car l))
          (loop (add1 i) (cdr l))))))

(define (encode-var-addr v)
  (if (and (pair? v) (eq? (car v) 'global))
      (set-nth-bit (cadr v) 15)
      (cadr v)))

(define (global-var-addr? v)
  (equal? 1 (get-nth-bit v 15)))

(define (decode-var-addr v)
  (clear-nth-bit v 15))

(define (constant->bytes obj)
  (cond
    [(null? obj) (list 'NULL)]
    [(eq? obj (void)) (list 'VOID)]
    [(boolean? obj)
     (if obj
         (list 'TRUE)
         (list 'FALSE))]
    [(and (number? obj) (< obj 0))
     (cons 'NEGINT (list-fill-to (number->byte-list (- obj)) INT-SZ 0))]
    [(number? obj) 
     (cons 'INT (list-fill-to (number->byte-list obj) INT-SZ 0))]
    [(char? obj)
      (list 'CHAR (char->integer obj))]
    [(string? obj)
     `(STR ,(string-length obj) ,@(bytevector->u8-list (string->bytevector obj (native-transcoder))))]
    [(symbol? obj)
     (let ([obj (symbol->string obj)])
      `(SYM . ,(cdr (constant->bytes obj))))]
    [else (error constant->bytes 'unknown:object obj)]))


;;; unnest
(define (appends xss)
  (let loop ([xss xss])
    (if (pair? xss)
        (append (car xss) (loop (cdr xss)))
        '())))

(define (encode-var-addr v)
  (if (and (pair? v) (eq? (car v) 'global))
      (set-nth-bit (cadr v) 15)
      (cadr v)))

(define (global-var-addr? v)
  (equal? 1 (get-nth-bit v 15)))

(define (decode-var-addr v)
  (clear-nth-bit v 15))

(define (encode-fn-arity n improper?)
  (logor n (if improper? (expt 2 7) 0)))

(define (decode-fn-arity n)
  (list (logand n (sub1 (expt 2 7))) (if (eq? (ash n -7) 1) #t #f)))

(define (unnest-code ln)
  (match ln
    [(CONSTANT ,v)
     (constant->bytes v)]
    [(,t ,e)
     (guard (member t
        '(NULL TRUE FALSE NEGINT INT STR SYM CHAR)))
     `(,t . ,(cdr (constant->bytes e)))]
    [(FUNCTION ,ptr ,free-vars ,arity)
     `(FUNCTION
        ,@(if (number? ptr) (list-fill-to (number->byte-list ptr) JMP-SZ 0) (list ptr))
        ,(length free-vars)
        ,@(appends (map (lambda (v) (list-fill-to (number->byte-list (encode-var-addr v)) 2 0)) free-vars))
        ,(encode-fn-arity (car arity) (cadr arity)))]
    [(IF ,v) (guard (number? v))
     `(IF . ,(list-fill-to (number->byte-list v) JMP-SZ 0))]
    [(GOTO ,v) (guard (number? v))
     `(GOTO . ,(list-fill-to (number->byte-list v) JMP-SZ 0))]
    [(,t ,v)
     (guard (member t
        '(LOCAL-FETCH LOCAL-SET! GLOBAL-FETCH GLOBAL-SET!)))
     `(,t . ,(list-fill-to (number->byte-list v) 2 0))]
    [(,t . ,())
     (guard
      (member t
        '(PUSH-ENV PUSH-PROC PUSH-VAL POP-PROC POP-ENV
          NULL TRUE FALSE NEGINT INT STR SYM
          ; LOCAL-FETCH LOCAL-SET! GLOBAL-FETCH GLOBAL-SET!
          RETURN TAIL-CALL-PROC CALL-PROC CONS-PROC-VAL NULL
          LOAD FINISH
          IF LABEL GOTO)))
     ln]
    [,() (error unnest-code 'unknown:ln ln)]))

(define (unnest e)
  (map unnest-code e))

;;; replace-label
(define (label->addr lbl corspd)
  (list-fill-to (number->byte-list (cadr (assoc lbl corspd))) JMP-SZ 0))

(define (instr-length i)
  (match i
    [(FUNCTION . ,())
     (+ JMP-SZ (length (cdr i)))]
    [(IF . ,()) (add1 JMP-SZ)]
    [(GOTO . ,()) (add1 JMP-SZ)]
    [(,t . ,())
     (guard (member t
        '(PUSH-ENV PUSH-PROC PUSH-VAL POP-PROC POP-ENV
          NULL TRUE FALSE NEGINT INT STR SYM CHAR
          LOCAL-FETCH LOCAL-SET! GLOBAL-FETCH GLOBAL-SET!
          RETURN TAIL-CALL-PROC CALL-PROC CONS-PROC-VAL NULL
          LOAD FINISH)))
     (length i)]
    [,() (error instr-length 'unknown i)]))

;;; encode
(define instr-code-tbl
  (precompute-rank (map car (cdr (table-row-fetch instr-interp-tbl 'sexp-interp)))))

(define (instr->op-code i)
  (let ([r (assoc (car i) instr-code-tbl)])
    (if r
        (cons (cadr r) (cdr i))
        (error instr->op-code 'unknown i))))
#;(for-each (lambda (s) (pretty-print (list 'quote s))) '(PUSH-ENV PUSH-PROC PUSH-VAL POP-PROC POP-ENV NULL TRUE FALSE NEGINT INT STR SYM CHAR LOCAL-FETCH LOCAL-SET! GLOBAL-FETCH GLOBAL-SET! RETURN TAIL-CALL-PROC CALL-PROC CONS-PROC-VAL NULL LOAD FINISH FUNCTION IF GOTO))
;;; decode
(define (load-bytecode-decode)
  (table-add! instr-interp-tbl 'PUSH-ENV 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(PUSH-ENV))))
  (table-add! instr-interp-tbl 'PUSH-PROC 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(PUSH-PROC))))
  (table-add! instr-interp-tbl 'PUSH-VAL 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(PUSH-VAL))))
  (table-add! instr-interp-tbl 'POP-PROC 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(POP-PROC))))
  (table-add! instr-interp-tbl 'POP-ENV 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(POP-ENV))))
  (table-add! instr-interp-tbl 'NULL 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(NULL))))
  (table-add! instr-interp-tbl 'TRUE 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(TRUE))))
  (table-add! instr-interp-tbl 'FALSE 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(FALSE))))
  (table-add! instr-interp-tbl 'NEGINT 'bytecode-decode
    (lambda (prog-ref)
      (cons INT-SZ
            (list 'NEGINT
                  (byte-list->number
                    (map prog-ref (iota INT-SZ)))))))
  (table-add! instr-interp-tbl 'INT 'bytecode-decode
    (lambda (prog-ref)
      (cons INT-SZ
            (list 'INT
                  (byte-list->number
                    (map prog-ref (iota INT-SZ)))))))
  (table-add! instr-interp-tbl 'STR 'bytecode-decode
    (lambda (prog-ref)
      (let* ([str-len (prog-ref 0)]
             [cs (map add1 (iota str-len))]
             [cs (map prog-ref cs)]
             [s (list->string (map integer->char cs))])
        (cons (add1 str-len) (list 'STR s)))))
  (table-add! instr-interp-tbl 'SYM 'bytecode-decode
    (lambda (prog-ref)
      (let* ([str-len (prog-ref 0)]
             [cs (map add1 (iota str-len))]
             [cs (map prog-ref cs)]
             [s (list->string (map integer->char cs))])
        (cons (add1 str-len) (list 'SYM (string->symbol s))))))
  (table-add! instr-interp-tbl 'CHAR 'bytecode-decode
    (lambda (prog-ref)
      (cons 1 (list 'CHAR (integer->char (prog-ref 0))))))
  (table-add! instr-interp-tbl 'LOCAL-FETCH 'bytecode-decode
    (lambda (prog-ref)
      (cons 2
           (list 'LOCAL-FETCH (byte-list->number (list (prog-ref 0) (prog-ref 1)))))))
  (table-add! instr-interp-tbl 'LOCAL-SET! 'bytecode-decode
    (lambda (prog-ref)
      (cons 2
           (list 'LOCAL-SET! (byte-list->number (list (prog-ref 0) (prog-ref 1)))))))
  (table-add! instr-interp-tbl 'GLOBAL-FETCH 'bytecode-decode
    (lambda (prog-ref)
      (cons 2
           (list 'GLOBAL-FETCH (byte-list->number (list (prog-ref 0) (prog-ref 1)))))))
  (table-add! instr-interp-tbl 'GLOBAL-SET! 'bytecode-decode
    (lambda (prog-ref)
      (cons 2
           (list 'GLOBAL-SET! (byte-list->number (list (prog-ref 0) (prog-ref 1)))))))
  (table-add! instr-interp-tbl 'RETURN 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(RETURN))))
  (table-add! instr-interp-tbl 'TAIL-CALL-PROC 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(TAIL-CALL-PROC))))
  (table-add! instr-interp-tbl 'CALL-PROC 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(CALL-PROC))))
  (table-add! instr-interp-tbl 'CONS-PROC-VAL 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(CONS-PROC-VAL))))
  (table-add! instr-interp-tbl 'NULL 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(NULL))))
  (table-add! instr-interp-tbl 'LOAD 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(LOAD))))
  (table-add! instr-interp-tbl 'FINISH 'bytecode-decode
    (lambda (prog-ref)
      (cons 0 '(FINISH))))
  (table-add! instr-interp-tbl 'FUNCTION 'bytecode-decode
    (lambda (prog-ref)
      (let* ([ptr (byte-list->number (map prog-ref (iota JMP-SZ)))]
             [var-n (prog-ref JMP-SZ)]
             [vars (map (lambda (i) (prog-ref (+ (add1 JMP-SZ) i))) (iota (* 2 var-n)))]
             [vars
              (let loop ((vars vars))
                (if (null? vars)
                    '()
                    (cons (list (car vars) (cadr vars)) (loop (cddr vars)))))]
             [vars (map byte-list->number vars)]
            ;  [vars (map decode-var-addr vars)]
             ;;; TODO: it is a workaround?
             [vars (map (lambda (v) (list (if (global-var-addr? v) 'global 0)
                                    (decode-var-addr v)))
                        vars)]
             [arity (decode-fn-arity (prog-ref (+ (add1 JMP-SZ) (* 2 var-n))))]
             )
        (cons (+ 1 (add1 JMP-SZ) (* 2 var-n)) `(FUNCTION ,ptr ,vars ,arity)))))
  (table-add! instr-interp-tbl 'IF 'bytecode-decode
    (lambda (prog-ref)
      (cons JMP-SZ (list 'IF (byte-list->number (map prog-ref (iota JMP-SZ)))))))
  (table-add! instr-interp-tbl 'GOTO 'bytecode-decode
    (lambda (prog-ref)
      (cons JMP-SZ (list 'GOTO (byte-list->number (map prog-ref (iota JMP-SZ)))))))
  )
(load-bytecode-decode)

(map
  (lambda (bs)
    (let* ([x (car bs)]
           [decode (cadr bs)]
           [interp (table-lookup instr-interp-tbl x 'sexp-interp)]
           [reloc (table-lookup instr-interp-tbl x 'sexp-relocate)])
    (table-add! instr-interp-tbl x 'bytecode-interp
      (lambda (opcode)
        (let* ([p (decode (lambda (i) (vector-ref PROG (+ PC 1 i))))]
               [i (car p)]
               [ln (cdr p)])
          ((interp (add1 i)) ln)
          )))
    (table-add! instr-interp-tbl x 'bytecode-relocate
      (lambda (opcode offset env)
        (let* ([p (decode (lambda (i) (vector-ref PROG (+ PC 1 i))))]
               [i (car p)]
               [ln (cdr p)]
               [e (reloc ln offset env)]
               [e* (instr->op-code (unnest-code e))])
          (vector-list-set! PROG e* PC)
          (set! PC (+ (add1 i) PC))
          )))
    ))
  (cdr (table-row-fetch instr-interp-tbl 'bytecode-decode)))

(define (interp-code)
  (define tbl
    (cdr (table-row-fetch instr-interp-tbl 'bytecode-interp)))
  (define (find-instr i)
    (cadr (list-ref tbl i)))
  (let loop ()
    (if (< PC 0)
        'done
        (let* ([ln (vector-ref PROG PC)]
               [f (find-instr ln)])
          (f ln)
          (loop)))))

(define (relocate-prog! prog offset env)
  (define end (vector-length prog))
  (define tbl (cdr (table-row-fetch instr-interp-tbl 'bytecode-relocate)))
  (define (find-instr i)
    (cadr (list-ref tbl i)))
  (define prev-pc PC)
  (let loop ()
    (if (< PC end)
        (let* (
          [ln (vector-ref prog PC)]
          [f-set! (find-instr ln)])
          (f-set! ln offset env)
          (loop))
        'done))
  (set! PC prev-pc)
  )

(define (interp-top e)
  (let* 
    ([e* (desugar e)]
     [e* (desugar-op e*)]
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
     [IR (append IR '((FINISH)))]
     [IR (unnest IR)]
     [IR (replace-label-top IR)]
     [IR (map instr->op-code IR)]
     [IR (list->vector (appends IR))]
    ;  [_ (write-sexps (generate-filename ".tmp/" ".scm") IR)]
    ;  [_ (write-bytevector-into-file (generate-filename ".tmp/" ".so") IR)]
     [offset (vector-length PROG)]
     )
    (set! ENV GLOBAL.FRAME)
    (set! PROG (vector-append PROG IR))
    (set! PC offset)
    (relocate-prog! PROG offset id-env)
    (interp-code)
    VAL
  ))
