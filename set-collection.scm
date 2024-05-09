(define (list->set xs)
  (if (null? xs)
      '()
      (set-add-elem (list->set (cdr xs)) (car xs))))

(define (make-set . xs)
  (list->set xs))

(define (set-add-elem xs x)
  (if (member x xs)
      xs
      (cons x xs)))

(define (set-union xs ys)
  (if (null? xs)
      ys
      (set-union (cdr xs) (set-add-elem ys (car xs)))))

(define (set-diff xs ys)
  (let loop ([xs xs] [res '()])
    (if (null? xs)
        res
        (loop (cdr xs) (if (member (car xs) ys) res (cons (car xs) res))))))

(define (set-intersection xs ys)
  (set-diff ys (set-diff ys xs)))

