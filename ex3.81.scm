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

(define random-init 0)
(define (rand-update x)
  (modulo (+ (* 132417 x) 141341437) 114329))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))


(define (make-random-numbers init)
  (define random-numbers
    (cons-stream init
                 (stream-map rand-update random-numbers)))
  random-numbers)


(define (generator input-stream)
  (define (inner input rands)
    (let ((instruction (stream-car input)))
      (if (null? instruction)
        the-empty-stream
        (if (eq? (car instruction) 'generate)
          (cons-stream (stream-car rands)
                       (inner (stream-cdr input)
                              (stream-cdr rands)))
          (let ((new-init (cadr instruction)))
            (inner (stream-cdr input)
                   (make-random-numbers new-init)))))))
  (inner input-stream (make-random-numbers 0)))

(define instructions
  (cons-stream '(reset 30793) (cons-stream '(generate) (cons-stream '(generate) (cons-stream '(generate) (cons-stream '(reset 30793) (cons-stream '(generate) (cons-stream '(generate) (cons-stream '(generate) the-empty-stream)))))))))

(display-stream (generator instructions))
