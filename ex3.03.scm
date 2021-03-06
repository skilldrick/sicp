(define (make-account balance password)
  (define tries 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (bad-password amount)
    (set! tries (+ tries 1))
    (if (> tries 7)
        (call-the-cops)
        "Incorrect password"))
  (define (dispatch m p)
    (if (eq? password p)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        bad-password))
  dispatch)

(define acct (make-account 100 'blah))

((acct 'withdraw 'blah) 50)

((acct 'deposit 'bah) 29)
