(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define cyc (make-cycle (list 'a 'b 'c)))

(define non-cycle '(a b c f))

(define (check-cycles x)
  (define pairs '())
  (define (inner x)
    (cond ((not (pair? x)) #f)
          ((memq x pairs) #t)
          (else
           (set! pairs (cons x pairs))
           (or (inner (car x))
               (inner (cdr x))))))
  (inner x))

(check-cycles non-cycle)

(check-cycles cyc)
