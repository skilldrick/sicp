(define random-init 0)
(define (rand-update x)
  (modulo (+ (* 132417 x) 141341437) 114329))

(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (new-init)
               (set! x new-init)))
            (else
             "Bad message")))
    dispatch))

(rand 'generate)

((rand 'reset) 16)
