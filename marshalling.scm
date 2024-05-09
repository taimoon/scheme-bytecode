(define bit-length 8)

(define (clear-nth-bit n i)
  (logand n (lognot (ash 1 i))))

(define (set-nth-bit n i)
  (logor n (ash 1 i)))

(define (get-nth-bit n i)
  (logand (ash n (- i)) 1))

(define (number->bit-list n)
  (map (lambda (c) (if (equal? c #\1) 1 0))
       (string->list (number->string n 2))))
  
(define (bit-list->number bs)
  (string->number
    (list->string
      (map (lambda (n) (if (equal? n 1) #\1 #\0))
           bs))
    2))

(define (list-duplicate n v)
  (if (<= n 0)
      '()
      (cons v (list-duplicate (sub1 n) v))))

(define (list-fill-to xs n v)
  (let ([l (length xs)])
      (if (>= l n)
          xs
          (append xs (list-duplicate (- n l) v)))))

(define (number->byte-list n)
  (if (equal? n 0)
      '(0)
      (let loop ([n n])
        (if (equal? n 0)
          '()
          (cons 
            (logand n (sub1 (ash 1 bit-length)))
            (loop (ash n (- bit-length))))))))

(define (byte-list->number ns)
  (if (null? ns)
      0
      (logor (car ns)
             (ash (byte-list->number (cdr ns)) bit-length))))
