(define (ripple-carry-adder ak bk sk c)
  (if (null? ak)
      (set-signal! c 0)
      (let ((ck (make-wire)))
        (full-adder (car ak)
                    (car bk)
                    ck
                    (car sk)
                    c)
        (ripple-carry-adder (cdr ak)
                            (cdr bk)
                            (cdr sk)
                            ck))))

