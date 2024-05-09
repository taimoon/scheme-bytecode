(define interpreter-file
  (let ([args (command-line)])
    (if (and (pair? args) (pair? (cdr args)))
        (cadr args)
        "interpreter.scm")))

(pretty-print '____________)
(load interpreter-file)
(interp* `(begin
    (load "top-prog.scm")
    (pretty-print (apply + '(1 2 3)))
    (pretty-print (read-sexps "test-meta-lisp.scm"))
    (pretty-print 'loading-meta)
    (load ,interpreter-file)
    (pretty-print 'end-of-loading)
    (load "test-lisp.scm")
  ))