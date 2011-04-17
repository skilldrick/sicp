(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define w '(a b c))

(count-pairs w)

(define b '(b))

(define x (cons (cons 'a b)
                b))

(count-pairs x)

(define level0 '(a))

(define level1 (cons level0 level0))

(define y (cons level1 level1))

(display (count-pairs y))

(define z (mcons 'a (mcons 'b (mcons 'c '()))))

(set-mcdr! (mcdr (mcdr z)) z)

z
