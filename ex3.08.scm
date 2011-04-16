(define f
  (let ((prev 1))
    (lambda (x)
      (set! prev (* x prev))
      prev)))



(+ (f 0) (f 1))

(+ (f 1) (f 0))
