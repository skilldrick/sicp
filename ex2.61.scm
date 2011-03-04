(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set))))) 

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2))))))) 

(define (adjoin-set x set)
  (cond ((null? set) '())
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set))
         (cons (car set)
               (adjoin-set x (cdr set)))))) 

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2))
         '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2
                        (union-set set1 (cdr set2)))))))))  
  
(union-set '(1 2 3) '(2 3 4)) 
