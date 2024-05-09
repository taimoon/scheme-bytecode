(define (write-sexps filename sexps)
  (call-with-port 
    (open-output-file filename)
      (lambda (op)
        (let loop ([sexps sexps])
          (if (null? sexps)
              (void)
              (begin (pretty-print (car sexps) op) (loop (cdr sexps))))))))

(define (read-sexps filename)
  (call-with-port 
    (open-input-file filename)
      (lambda (ip)
        (let loop ([sexps '()])
          (let ((sexp (read ip)))
              (if (eof-object? sexp)
                  (reverse sexps)
                  (loop (cons sexp sexps))))))))

(define (write-bytevector-into-file loc vs)
  (call-with-port 
    (open-file-output-port loc)
    (lambda (op)
      (put-bytevector-some op (u8-list->bytevector (vector->list vs))))))

(define (read-bytevector-from-file loc)
  (call-with-port 
    (open-file-input-port loc)
    (lambda (ip)
      (list->vector (bytevector->u8-list (get-bytevector-some ip))))))
