(define (make-accumulator total)
  (lambda (n)
    (set! total (+ total n))
    total))

(define A (make-accumulator 5))

(A 10)
