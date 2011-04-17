(define (count-pairs x)
  (define pairs '())
  (define (inner x)
    (if (not (pair? x)) 'none
        (begin
          (if (memq x pairs)
              'none
              (set! pairs (cons x pairs)))
          (inner (car x))
          (inner (cdr x)))))
  (inner x)
  (length pairs))


(define w '(a b c))

(count-pairs w)

(define b '(b))

(define x (cons (cons 'a b)
                b))

(count-pairs x)

(define level0 '(a))

(define level1 (cons level0 level0))

(define y (cons level1 level1))

(count-pairs y)
