(define (entry tree) (car tree)) 
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 
(define (make-tree entry left right)
  (list entry left right)) 

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))



(define tree-a (make-tree 7
                          (make-tree 3
                                     (make-tree 1 '() '())
                                     (make-tree 5 '() '()))
                          (make-tree 9
                                     '()
                                     (make-tree 11 '() '()))))

(define tree-b (make-tree 5
(make-tree 3
(make-tree 1 '() '())
(make-tree 4 '() '()))
(make-tree 6 '() '())))

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

(define (union-set-tree set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (let ((result-list (union-set list1 list2)))
      (list->tree result-list))))

(union-set-tree tree-a tree-b)
      
(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (let ((result-list (intersection-set list1 list2)))
      (list->tree result-list))))
  
(intersection-set-tree tree-a tree-b)
