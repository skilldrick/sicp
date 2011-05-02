(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with empty queue")
          (car front-ptr)))
    (define (print)
      front-ptr)
    (define (insert! item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (print))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               (print)))))
    (define (delete!)
      (cond ((empty?)
             (error "DELETE called with empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             (print))))
    (define (dispatch m)
      (cond ((eq? m 'front) front)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print)
            ((eq? m 'delete!) delete!)
            (else
             (error "Bad message for dispatch" m))))
    dispatch))

(define q (make-queue))
      
(define (front-queue q)
  ((q 'front)))

(define (insert-queue q item)
  ((q 'insert!) item))

(define (print-queue q)
  ((q 'print)))

(define (delete-queue! q)
  ((q 'delete!)))

(insert-queue q 'a)

(insert-queue q 'b)

(delete-queue! q)
