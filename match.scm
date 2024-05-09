(define-syntax match
  (syntax-rules ()
    ((_ e . clauses)
     (let ((value e))
      (match-evaluated value . clauses)))))

(define-syntax match-evaluated
  (syntax-rules (guard)
    ((_ value)
     (error 'match 'no-matching-clauses value))
    ((_ value . ((pattern (guard predicate) . expressions) . clauses))
     (match-clause
        ((pattern value))
        (and)
        ()
        expressions
        (match-evaluated value . clauses)
        predicate))
    ((_ value . ((pattern . expressions) . clauses))
     (match-clause
        ((pattern value))
        (and)
        ()
        expressions
        (match-evaluated value . clauses)
        #t))))

(define-syntax match-clause
  (syntax-rules (unquote)
    #;(match-clause pairs condition bindings expressions alternative predicate)
    ((_ () condition bindings expressions alternative #t)
     (if condition
          (let bindings . expressions)
          alternative))
    ((_ () condition bindings expressions alternative predicate)
     (let ([alter (lambda () alternative)])
        (if condition
          (let bindings (if predicate (begin . expressions) (alter)))
          (alter))))
    ;;; underscore is reserved for syntax-rules
    ;;; underscore cannot be used as empty pattern
    ;;; instead, any non-identifier can be used. ,() is a great choice
    ((_ ((,() root) . rest) condition bindings expressions alternative predicate)
     (match-clause 
      rest
      condition
      bindings
      expressions
      alternative
      predicate))
    ((_ ((,variable root) . rest) condition bindings expressions alternative predicate)
     (match-clause 
      rest
      condition
      ((variable root) . bindings)
      expressions
      alternative
      predicate))
    ((_ (((left . right) root) . rest) (and condition ...) bindings expressions alternative predicate)
     (match-clause 
      ((left (car root)) (right (cdr root)) . rest)
      (and condition ... (pair? root))
      bindings
      expressions
      alternative
      predicate))
    ((_ ((literal root) . rest) (and condition ...) bindings expressions alternative predicate)
     (match-clause 
      rest
      (and condition ... (equal? (quote literal) root))
      bindings
      expressions
      alternative
      predicate))))

#;(begin
  'example
  (define (interp e)
    (match e
      (,v (guard (number? v)) v)
      ((+ ,e1 ,e2) (+ (interp e1) (interp e2)))
      ((- ,e1 ,e2) (- (interp e1) (interp e2)))
      ((* ,e1 ,e2) (* (interp e1) (interp e2)))
      ((/ ,e1 ,e2) (/ (interp e1) (interp e2)))
      (,() (error interp 'unmatch:e e))))

  (pretty-print (interp '
    (+ (* 3 3) (* 4 4))
)))