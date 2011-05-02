(define (make-node data) (list '() data '()))
(define (prev-ptr node) (car node))
(define (next-ptr node) (caddr node))
(define (data node) (cadr node))
(define (set-prev-ptr! node prev-node)
  (set-car! node prev-node))
(define (set-next-ptr! node next-node)
  (set-car! (cddr node) next-node))

(define (front-ptr d) (car d))
(define (rear-ptr d) (cdr d))
(define (set-front-ptr! d node) (set-car! d node))
(define (set-rear-ptr! d item) (set-cdr! d item))

(define (empty-deque? d) (null? (front-ptr d)))
(define (make-deque) (cons '() '()))

(define (front-deque d)
  (if (empty-deque? d)
      (error "FRONT called with an empty deque" d)
      (car (front-ptr d))))

(define (rear-deque d)
  (if (empty-deque? d)
      (error "REAR called with an empty deque" d)
      (car (rear-ptr d))))

(define (show-deque d)
  (define (inner lst)
    (cond ((null? lst) 'done)
          (else
           (display (data lst))
           (newline)
           (inner (next-ptr lst)))))
  (inner (front-ptr d)))


(define (rear-insert-deque! d item)
  (let ((new-node (make-node item)))
    (cond ((empty-deque? d)
           (set-front-ptr! d new-node)
           (set-rear-ptr! d new-node)
           (show-deque d))
          (else
           (set-prev-ptr! new-node (rear-ptr d))
           (set-next-ptr! (rear-ptr d) new-node)
           (set-rear-ptr! d new-node)
           (show-deque d)))))

(define (front-insert-deque! d item)
  (let ((new-node (make-node item)))
    (cond ((empty-deque? d)
           (set-front-ptr! d new-node)
           (set-rear-ptr! d new-node)
           (show-deque d))
          (else
           (set-prev-ptr! (front-ptr d) new-node)
           (set-next-ptr! new-node (front-ptr d))
           (set-front-ptr! d new-node)
           (show-deque d)))))

(define (front-delete-deque! d)
  (cond ((empty-deque? d)
         (error "FRONT-DELETE! called with an empty deque" d))
        (else
         (set-front-ptr! d (next-ptr (front-ptr d)))
         (set-prev-ptr! (front-ptr d) '())
         (show-deque d))))

(define (rear-delete-deque! d)
  (cond ((empty-deque? d)
         (error "REAR-DELETE! called with an empty deque" d))
        (else
         (set-rear-ptr! d (prev-ptr (rear-ptr d)))
         (set-next-ptr! (rear-ptr d) '())
         (show-deque d))))

(define d (make-deque))
(front-insert-deque! d 'a)
(rear-insert-deque! d 'c)
(front-insert-deque! d 'd)
(rear-insert-deque! d 'e)
(front-delete-deque! d)
(rear-delete-deque! d)
