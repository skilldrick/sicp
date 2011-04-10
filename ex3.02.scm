(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) calls)
            ((eq? arg 'reset-count) (set! calls 0))
            (else
             (set! calls (+ calls 1))
             (f arg))))))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
