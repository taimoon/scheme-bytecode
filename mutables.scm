(define (make-box v)
  (lambda (msg)
    (case msg
      [(set!) (lambda (obj) (set! v obj))]
      [(val) v]
      [else (error make-box 'unknown msg)])))
(define (box-set! b v) ((b 'set!) v))
(define (box-val b) (b 'val))

(define (make-frame xs)
  (let ([xs (list->vector xs)])
    (lambda (msg)
      (case msg
        [(set!) (lambda (n obj) (vector-set! xs n obj))]
        [(ref) (lambda (n) (vector-ref xs n))]
        [(append!)
          (lambda (ys)
            (cond [(list? ys)
                   (set! xs (vector-append xs (list->vector ys)))]
                  [(vector? ys)
                   (set! xs (vector-append xs ys))]
                  [else (error make-frame 'append!:TypeError ys)]))]
        [else (error make-frame 'unknown msg)]))))

(define (frame-ref arr n)
  ((arr 'ref) n))

(define (frame-set! arr n v)
  ((arr 'set!) n v))

(define (frame-append! arr xs)
  ((arr 'append!) xs))

(define TABLE-TAG (gensym "table"))
(define (table? table)
  (and (pair? table)
       (eq? (car table) TABLE-TAG)))
(define (create-table) (list (list TABLE-TAG)))
(define (table-assoc table k1 k2)
  #;(column row)
  (let ([tbl (assoc k1 table)])
    (if tbl
        (let ([record (assoc k2 (cdr tbl))])
          (if record
              record
              #f))
        #f)))
(define (table-lookup table k1 k2)
  (let ([record (table-assoc table k1 k2)])
    (if record
        (cdr record)
        (error table-lookup 'unbound-keys k1 k2))))
(define (table-row-fetch table k2)
  (cons k2
    (map (lambda (bs) 
          (let ([r (table-assoc table (car bs) k2)])
            (if r
                (list (car bs) (cdr r))
                (list (car bs) r))
            )
    #;(list (car bs) (cdr (assoc k2 (cdr bs)))))
         (cdr table))))
(define (table-add! table k1 k2 v)
  (let ([tbl (assoc k1 table)])
    (if tbl
        (let ([record (assoc k2 (cdr tbl))])
          (if record
              (set-cdr! record v)
              (append! tbl (list (cons k2 v)))))
        (append! table (list (list k1 (cons k2 v)))))))

(define (table-add-row! tbl k1 corspd)
  (map (lambda (c) (table-add! tbl k1 (car c) (cadr c))) corspd))