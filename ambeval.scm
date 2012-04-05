#lang r5rs

(define (error . args)
  (newline)
  (display args))

(define apply-in-underlying-scheme apply)

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((amb? exp) (analyze-amb exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((let? exp) (analyze (let->combination exp)))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence
                        (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type - ANALYZE" exp))))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))


(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence - ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                       (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
                  (lambda (arg fail2)
                    (get-args (cdr aprocs)
                              env
                              (lambda (args fail3)
                                (succeed (cons arg args)
                                         fail3))
                              fail2))
                  fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
          (error "Unknown procedure type - EXECUTE-APPLICATION" proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next cprocs))))

;(define (analyze-lambda exp)
;  (let ((vars (lambda-parameters exp))
;        (bproc (analyze-sequence (lambda-body exp))))
;    (lambda (env) (make-procedure vars bproc env))))
;
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;    '()
;    (cons (eval (first-operand exps) env)
;          (list-of-values (rest-operands exps) env))))
;
;(define (and? exp)
;  (tagged-list? exp 'and))
;
;(define (or? exp)
;  (tagged-list? exp 'or))
;
;(define (eval-and exp env)
;  (let ((first (eval (cadr exp) env)))
;    (if (eq? first #f)
;      first
;      (eval (caddr exp) env))))
;
;(define (eval-or exp env)
;  (let ((first (eval (cadr exp) env)))
;    (if (eq? first #f)
;      (eval (caddr exp) env)
;      first)))
;
;(define (eval-if exp env)
;  (if (true? (eval (if-predicate exp) env))
;    (eval (if-consequent exp) env)
;    (eval (if-alternative exp) env)))
;
;(define (eval-sequence exps env)
;  (cond ((last-exp? exps) (eval (first-exp exps) env))
;        (else (eval (first-exp exps) env)
;              (eval-sequence (rest-exps exps) env))))
;
;(define (eval-assignment exp env)
;  (set-variable-value! (assignment-variable exp)
;                       (eval (assignment-value exp) env)
;                       env)
;  'assignment-ok)
;
;(define (eval-definition exp env)
;  (define-variable! (definition-variable exp)
;                    (eval (definition-value exp) env)
;                    env)
;  'definition-ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last = COND->IF"
                 clauses))
        (if (and (pair? (cond-actions first)) (eq? (car (cond-actions first)) '=>))
          (make-if (cond-predicate first)
                   (list (cadr (cond-actions first)) (cond-predicate first))
                   (expand-clauses rest))
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest)))))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (define (vars pairs)
    (map car pairs))
  (define (vals pairs)
    (map cadr pairs))
  (let ((let-pairs (cadr exp))
        (body (cddr exp)))
    (append
      (list (make-lambda (vars let-pairs) body))
      (vals let-pairs))))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (let ((let-pairs (cadr exp))
        (let-body (caddr exp)))
    (define (iter pairs)
      (if (null? pairs)
        let-body
        (list 'let
              (list (car pairs))
              (iter (cdr pairs)))))
    (iter let-pairs)))

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((let-pairs (cadr exp))
        (let-body (caddr exp)))
    (let ((let-vars (map car let-pairs))
          (let-vals (map cadr let-pairs)))
      (append (list 'let
                    (map (lambda (var)
                           (list var ''*unassigned*))
                         let-vars))
              (map (lambda (pair)
                     (list 'set! (car pair) (cadr pair)))
                   let-pairs)
              (list let-body)))))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned)
               (error "Unassigned variable:" var)
               (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
              (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define (initial-eval exp)
      (ambeval exp
               initial-env
               (lambda (val next) val)
               (lambda () 'fail)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    (initial-eval '(define (not x) (if x false true)))
    (initial-eval '(define (xor a b)
                     (if a (not b) b)))
    (initial-eval '(define (an-integer-from low)
                     (amb low (an-integer-from (+ low 1)))))
    (initial-eval '(define (require p)
                     (if (not p) (amb))))
    (initial-eval '(define (an-element-of items)
                     (require (not (null? items)))
                     (amb (car items) (an-element-of (cdr items)))))
    (initial-eval '(define (an-integer-between low high)
                     (require (< low high))
                     (amb low (an-integer-between  (+ low 1) high))))
    (initial-eval '(define (a-pythagorean-triple-between low high)
                     (let ((i (an-integer-between low high)))
                       (let ((j (an-integer-between i high)))
                         (let ((k (an-integer-between j high)))
                           (require (= (+ (* i i) (* j j)) (* k k)))
                           (list i j k))))))
    (initial-eval '(define (a-pythagorean-triple)
                     (let ((c (an-integer-from 1)))
                       (let ((b (an-integer-between 1 c)))
                         (let ((a (an-integer-between 1 b)))
                           (require (= (+ (* a a) (* b b)) (* c c)))
                           (list a b c))))))
    (initial-eval '(define (member item list)
                     (cond ((null? list) false)
                           ((equal? item (car list)) true)
                           (else (member item (cdr list))))))
    (initial-eval '(define (abs x)
                     (if (> x 0) x (- x))))
    (initial-eval '(define (distinct? items)
                     (cond ((null? items) true)
                           ((null? (cdr items)) true)
                           ((member (car items) (cdr items)) false)
                           (else (distinct? (cdr items))))))
    (initial-eval '(define (multiple-dwelling)
                     (let ((baker (amb 1 2 3 4 5))
                           (cooper (amb 1 2 3 4 5))
                           (fletcher (amb 1 2 3 4 5))
                           (miller (amb 1 2 3 4 5))
                           (smith (amb 1 2 3 4 5)))
                       (require
                         (distinct? (list baker cooper fletcher miller smith)))
                       (require (not (= baker 5)))
                       (require (not (= cooper 1)))
                       (require (not (= fletcher 5)))
                       (require (not (= fletcher 1)))
                       (require (> miller cooper))
                       (require (not (= (abs (- smith fletcher)) 1)))
                       (require (not (= (abs (- fletcher cooper)) 1)))
                       (list (list 'baker baker)
                             (list 'cooper cooper)
                             (list 'fletcher fletcher)
                             (list 'miller miller)
                             (list 'smith smith)))))
    (initial-eval '(define (liars)
                     (let ((betty (amb 1 2 3 4 5))
                           (ethel (amb 1 2 3 4 5))
                           (joan (amb 1 2 3 4 5))
                           (kitty (amb 1 2 3 4 5))
                           (mary (amb 1 2 3 4 5)))
                       (require
                         (distinct? (list betty ethel joan kitty mary)))
                       (require (xor (eq? kitty 2) (eq? betty 3)))
                       (require (xor (eq? ethel 1) (eq? joan 2)))
                       (require (xor (eq? joan 3) (eq? ethel 5)))
                       (require (xor (eq? kitty 2) (eq? mary 4)))
                       (require (xor (eq? mary 4) (eq? betty 1)))
                       (list (list 'betty betty)
                             (list 'ethel ethel)
                             (list 'joan joan)
                             (list 'kitty kitty)
                             (list 'mary mary)))))

    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list 'equal? equal?)
        (list 'eq? eq?)
        (list 'display display)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)
        (list '> >)
        (list '< <)
        (list '= =)
        ;; etc
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin
          (newline)
          (display ";;; Starting a new problem ")
          (ambeval input
                   the-global-environment
                   (lambda (val next-alternative)
                     (announce-output output-prompt)
                     (user-print val)
                     (internal-loop next-alternative))
                   (lambda ()
                     (announce-output ";;; There are no more values of")
                     (user-print input)
                     (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

(define the-global-environment (setup-environment))

(driver-loop)

;; this needs updating to amb evaluator
(define (test)
  (define (global-eval input)
    (eval input the-global-environment))
  (define (assert test-name input expected-output)
    (let ((result (global-eval input)))
      (if (equal? result expected-output)
        (display ".")
        (begin
          (display "x")
          (newline)
          (display "Failed ")
          (display test-name)
          (newline)
          (display "input: ")
          (display input)
          (newline)
          (display "expected: ")
          (display expected-output)
          (newline)
          (newline)))))

  (assert "Self-evaluting" '5 5)

  (newline)
)

;(test)
