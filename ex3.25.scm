(define (make-table)
  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))


  (define (lookup-recursive table-or-record keys)
    (if (null? keys)
        (cdr table-or-record)
        (let ((subtable (assoc (car keys) (cdr table-or-record))))
          (if subtable
              (lookup-recursive subtable (cdr keys))
              false))))

  (define (insert-recursive! table-or-record keys value)
    (cond ((null? keys)
           (set-cdr! table-or-record value)
          'ok)
          (else
           (let ((subtable (assoc (car keys) (cdr table-or-record))))
             (if subtable
                 (insert-recursive! subtable (cdr keys) value)
                 (let ((new-subtable (cons (car keys) '())))
                   (set-cdr! table-or-record
                             (cons new-subtable
                                   (cdr table-or-record)))
                   (insert-recursive! new-subtable (cdr keys) value)))))))

  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (lookup-recursive local-table keys))
    (define (insert! keys value)
      (insert-recursive! local-table keys value))
    (define (print) (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define tab (make-table))
(define get (tab 'lookup-proc))
(define put (tab 'insert-proc!))
(define print (tab 'print-proc))

(put '(a b c d) 5)
(print)

(get '(a b c d))

(put '(a f g h) 10)

(get '(a f g h))
