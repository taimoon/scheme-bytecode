(pretty-print "test-tail-call.scm")

(let ([args (command-line)])
  (if (and (pair? args) (pair? (cdr args)))
      (load (cadr args))
      (load "interpreter.scm")))

(define n 10000)

(pretty-print (interp* `
  (let ([sum-iter 0])
  (define (sum-iter n acm)
    (if (<= n 0)
        acm
        (sum-iter (- n 1) (+ n acm))))
  (print (cons '(sum ,n) (sum-iter ,n 0)))
  'done
  )
))

(pretty-print (interp* `
  (let ([sum 0])
  (define (sum n)
    (if (<= n 0)
        0
        (+ n (sum (- n 1)))))
  (print (cons '(sum ,n) (sum ,n)))
  'done
  )
))