#lang r5rs

(define (error . args)
  (newline)
  (display args))

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
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

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                            (map (lambda (aproc) (aproc env))
                                 aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
          (error "Unknown procedure type - EXECUTE-APPLICATION" proc))))


(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
        (cproc env)
        (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence - ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-and exp env)
  (let ((first (eval (cadr exp) env)))
    (if (eq? first #f)
      first
      (eval (caddr exp) env))))

(define (eval-or exp env)
  (let ((first (eval (cadr exp) env)))
    (if (eq? first #f)
      (eval (caddr exp) env)
      first)))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'assignment-ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'definition-ok)

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
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
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

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
      (if (eq? 'exit-loop output)
        (user-print "Goodbye!")
        (driver-loop)))))

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

;(driver-loop)

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
          (display "result: ")
          (display result)
          (newline)))))

  (global-eval '(define my-var 3))
  (global-eval '(define (func-with-inner-defines x)
                  (define a (+ x 1))
                  (define b (* x 2))
                  (define c (- x 1))
                  (+ a b c)))
  (global-eval '(define (func-with-inner-func x)
                  (define (inner-func y)
                    (* x y))
                  (inner-func 5)))
  (global-eval '(define (nested-set x)
                  (define (inner)
                    (set! x 10))
                  (inner)
                  x))
  (global-eval '(define (func1 x)
                  (* x 3)))
  (global-eval '(define (func2 x)
                  (let ((y (* x 2)))
                    (+ x y))))


  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)
  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)
  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)
  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)
  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)
  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)
  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)
  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (assert "Self-evaluting" '5 5)
  (assert "Variable"
          'my-var
          3)
  (assert "Quoted"
          ''(1 2 3)
          '(1 2 3))
  (global-eval '(define my-new-var 5))
  (global-eval '(set! my-new-var 7))
  (assert "Assignment" 'my-new-var 7)
  (assert "Inner defines"
          '(func-with-inner-defines 10)
          40)
  (assert "Inner function definitions"
          '(func-with-inner-func 6)
          30)
  (assert "Assignment to variable in enclosing scope"
          '(nested-set 5)
          10)
  (global-eval '(define my-new-var "Now I'm a string!"))
  (assert "Definition of previously defined variable"
          'my-new-var
          "Now I'm a string!")
  (assert "Let"
          '(let ((x 3))
             x)
          3)
  (assert "If consequent"
          '(if (> 5 3)
             "Maths works"
             "Parallel universe")
          "Maths works")
  (assert "If alternative"
          '(if (< 5 3)
             "Parallel universe"
             "Maths works")
          "Maths works")
  (assert "Lambda with application"
          '((lambda (x y)
              (* x y))
            3 4)
          12)
 (assert "Cond"
          '(cond ((> 1 100) "No way!")
                 ((= 3 5)
                  "This one has multiple statements"
                  "Are you crazy?")
                 ((< 55 5) "Don't be silly.")
                 ((> 5 3) "Finally, some sense.")
                 ((= (+ 2 2) 5) "Not unless it's 1984")
                 ((= (+ 2 2) 4) "This is true as well"))
          "Finally, some sense.")
  (assert "Alternative cond"
          '(cond ((cadr '((a 1) (b 2))) => cadr)
                 (else false))
          2)
  (assert "Application"
          '(+ 5 5)
          10)
  (assert "Application"
          '(- 10 5)
          5)
  (assert "Function definition and application"
          '(func1 3)
          9)
  (assert "Function with let"
          '(func2 2)
          6)
  (assert "Crazy Y-combinator thing factorial"
          '((lambda (n)
              ((lambda (fact)
                 (fact fact n))
               (lambda (ft k)
                 (if (= k 1)
                   1
                   (* k (ft ft (- k 1)))))))
            10)
          3628800)
  (assert "Crazy Y-combinator thing fib"
          '((lambda (n)
              ((lambda (fib)
                 (fib fib n))
               (lambda (f k)
                 (if (< k 2)
                   k
                   (+ (f f (- k 1))
                      (f f (- k 2)))))))
            10)
          55)

  (newline)
)

(test)
