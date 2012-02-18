#lang scheme

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (show x)
  (display-line x)
  x)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (take num stream)
  (if (= num 0)
    the-empty-stream
    (cons-stream (stream-car stream)
                 (take (- num 1)
                       (stream-cdr stream)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (sign-change-detector current-value previous-value)
  (cond ((and (>= current-value 0) (< previous-value 0))
         1)
        ((and (< current-value 0) (>= previous-value 0))
         -1)
        (else 0)))

(define (smooth str)
  (define (avg a b)
    (/ (+ a b) 2))
  (stream-map avg
              str
              (cons-stream 0 str)))

(define (make-zero-crossings input-stream)
  (let ((smoothed (smooth input-stream)))
    (stream-map sign-change-detector
                smoothed
                (cons-stream 0 smoothed))))


(define input-str
  (cons-stream 1 (cons-stream 2 (cons-stream 1 (cons-stream 0 (cons-stream -1 (cons-stream -2 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))))))))

(display-stream (make-zero-crossings input-str))
