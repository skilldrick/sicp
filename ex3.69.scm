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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))

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

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (interleave
        (stream-map
          (lambda (x) (list x (stream-car t)))
          (stream-cdr s))
        (pairs (stream-cdr s) (stream-cdr t))))))

(define (triples s t u)
  (stream-map (lambda (item)
                (list (caar item) (cadar item) (cadr item)))
              (pairs (pairs s t) u)))

;http://eli.thegreenplace.net/2007/11/10/sicp-section-353/
(define (eli-triples s1 s2 s3)
 (cons-stream
    (list
      (stream-car s1)
      (stream-car s2)
      (stream-car s3))
    (interleave
      (stream-map
        (lambda (x) (append (list (stream-car s1)) x))
        (stream-cdr (pairs s2 s3)))
      (eli-triples
        (stream-cdr s1)
        (stream-cdr s2)
        (stream-cdr s3)))))

(define (find-index stream item)
  (define (iter s index)
    (if (equal? (stream-car s) item)
      index
      (iter (stream-cdr s) (+ index 1))))
  (iter stream 1))

(define (square x) (* x x))

(define pythagorean-triples
  (stream-filter (lambda (triple)
                   (let ((i (car triple))
                         (j (cadr triple))
                         (k (caddr triple)))
                     (and (<= i j))
                          (= (square k) (+ (square i) (square j)))))
                 (eli-triples integers integers integers)))


(display-stream (take 10 (triples integers integers integers)))
(display-stream (take 10 (eli-triples integers integers integers)))
(display-stream (take 10 pythagorean-triples))
