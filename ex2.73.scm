;;Get and put code from:
;;http://eli.thegreenplace.net/2007/09/09/sicp-section-24/

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY GENERIC"
           (list op type-tags))))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp)
               var))))


(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-deriv-package)
  ;;internal procedures
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          (else (list '** b e))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  ;; interface to rest of system
  (put '* 'deriv 
       (lambda (operands var)
         (make-sum
          (make-product (multiplier operands)
                        (deriv (multiplicand operands) var))
          (make-product (deriv (multiplier operands) var)
                        (multiplicand operands)))))
  (put '+ 'deriv 
       (lambda (operands var)
         (make-sum (deriv (addend operands) var)
                   (deriv (augend operands) var))))
  (put '** 'deriv 
       (lambda (operands var)
         (make-product
          (make-product
           (exponent operands)
           (make-exponentiation (base operands)
                                (- (exponent operands) 1)))
          (deriv (base operands) var))))
  )


(install-deriv-package)

(deriv '(** x 5) 'x)

