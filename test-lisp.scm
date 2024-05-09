(pretty-print '===========)

(let ([args (command-line)])
  (if (and (pair? args) (pair? (cdr args)))
      (load (cadr args))
      (load "interpreter.scm")))

(interp* '(pretty-print '************))
(interp* '(pretty-print "testing time"))

(pretty-print 'simple)
(pretty-print (interp* '
  (- 5 -2)))
(pretty-print (interp* '
  (+ 1 2 3 4 5 6 7 8 9)))
(pretty-print (interp* '
  (+ (* 3 3) (* 4 4))))

(pretty-print 'if)
(pretty-print (interp* '
  (if (< 2 3) (+ 7 10) (div 1 0))
))

(pretty-print 'let)
(pretty-print (interp* '
  (let ([x 3] [y 4])
    (+ (* x x) (* y y)))
))
(pretty-print 'proc)
(pretty-print (interp* '
  (let ([x 3]
        [y 4]
        [sqr (lambda (x) (* x x))])
    (+ (sqr x) (sqr y)))
))
(pretty-print 'fact)
(pretty-print (interp* `
  (let ([fact 0])
    (set! fact
      (lambda (n)
        (if (<= n 1)
            1
            (* (fact (- n 1)) n))))
  (fact 10))
))
(pretty-print 'fact-iter-mutual)
(pretty-print (interp* `
  (let ([fact-iter 0] [fact 0])
    (set! fact-iter
      (lambda (n prod)
        (if (<= n 1)
            prod
            (fact-iter (- n 1) (* prod n)))))
    (set! fact
      (lambda (n) (fact-iter n 1)))
  (fact 10))
))
(pretty-print 'fib)
(pretty-print (interp* '
  (begin
    (define (fib n)
      (if (<= n 1) 
          1 
          (+ (fib (- n 1)) (fib (- n 2)))))
    (fib 10)
  )
))
(pretty-print 'data)
(pretty-print (interp* '
  (begin
    (define (filter pred xs)
      (if (pair? xs)
          (let ([x (car xs)]
                [ys (filter pred (cdr xs))])
            (if (pred x)
                (cons x ys)
                ys))
          '()))
    (define (even? x)
      (cond [(equal? x 0) #t]
            [(equal? x 1) #f]
            [else (even? (- x 2))]))
    (filter even? '(0 1 2 3 4 5 6 7))
  )
))
(pretty-print 'improper-argument)
(pretty-print (interp* '(begin
  (define (filter pred xs)
    (if (pair? xs)
        (let ([x (car xs)]
              [ys (filter pred (cdr xs))])
          (if (pred x)
              (cons x ys)
              ys))
        '()))
  (define (even? x)
    (cond [(equal? x 0) #t]
          [(equal? x 1) #f]
          [else (even? (- x 2))]))
  (define (odd? x) (not (even? x)))
  (define (same-parity x . xs)
    (cons x
      (if (even? x)
          (filter even? xs)
          (filter odd? xs))))
  (list (same-parity 1 2 3 4 5 6 7) (same-parity 2 3 4 5 6 7))
)))
(pretty-print 'scope)
(pretty-print (interp* '
  (let ()
    (pretty-print
      (let ([a 1])
        ((let ([a 2])
          (lambda (b) `(,a . ,b)))
         3))) ; (2 . 3) if lexcial, (1 . 3) if dynamic
    (let ([a 3])
      (let ([p (lambda (z) a)])
          (let ([f (lambda (x) (p 0))])
              (let ([a 5])
                  (f 2))))) ; 3 if lexical, 5 if dynamic
  )
))
(pretty-print (interp* '
  (let ()
    (define (interp** e)
      (match e
        ; (,v (guard (number? v)) v)
        ((+ ,e1 ,e2) (+ (interp** e1) (interp** e2)))
        ((- ,e1 ,e2) (- (interp** e1) (interp** e2)))
        ((* ,e1 ,e2) (* (interp** e1) (interp** e2)))
        ((/ ,e1 ,e2) (/ (interp** e1) (interp** e2)))
        (,_ e)))
    (cons 'interp** (interp** '(+ (* 3 3) (* 4 4))))
  )
))
(pretty-print 'let*)
(pretty-print (interp* '
  (let* ([x 3] [x (* x x)])
    x)
))
(pretty-print 'named-let)
(pretty-print (interp* '
  (let sum ([n 10])
    (if (<= n 0)
        0
        (+ n (sum (- n 1)))))))
(pretty-print 'closure)
(pretty-print (interp* '
  (let ()
    (define n 0)
    (define (add! . _) (set! n (+ n 1)) n)
    (define xs '(a b c d e f))
    (define (map f xs)
      (if (pair? xs)
          (cons (f (car xs)) (map f (cdr xs)))
          '()))
    (pretty-print (map add! xs))
    n)))
(pretty-print 'load)
(pretty-print (interp* '(begin
  (load "set-collection.scm")
  (define A '(1 2 3 4 5))
  (define B '(2 3 5 7 11))
  (set-union B A)
)))
