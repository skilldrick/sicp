(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define complex (make-from-real-imag 3 4))

(apply-generic 'imag-part complex)

(define (make-from-mag-ang magnitude angle)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* magnitude (cos angle)))
          ((eq? op 'imag-part)
           (* magnitude (sin angle)))
          ((eq? op 'magnitude) magnitude)
          ((eq? op 'angle) angle)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define complex2 (make-from-mag-ang 10 1))

(apply-generic 'real-part complex2)

