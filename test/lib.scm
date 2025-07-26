(define tests '())
(define n-failure 0)
(define n-success 0)
(define n-ignored 0)
(define test-name #f)

(define (describe name f)
  (set! tests `((,name . ,f) . ,tests)))
;;(define context describe)

(define (xdescribe name f);; ignored case
  (set! n-ignored (+ n-ignored 1)))

(define (display* . args)
  (for-each display args))

(define (msg-proc-1 msg)
  (lambda (x)
    (display* "<" x "> to be " msg)))

(define (msg-proc-2 msg)
  (lambda (x y)
    ((msg-proc-1 msg) x)
    (display* " to <" y ">")))

(define funcs
  '(> = equal? eq? eqv? boolean? even? integer? list? negative? null?
    number? odd? pair? port? positive? procedure? string? symbol? zero?))

(define fail-message-procs
  `((,> . ,(msg-proc-2 ">"))
    (,= . ,(msg-proc-2 "="))
    (,string=? . ,(msg-proc-2 "string=?"))
    (,equal? . ,(msg-proc-2 "equal?"))
    (,eq? . ,(msg-proc-2 "eq?"))
    (,eqv? . ,(msg-proc-2 "eqv?"))
    (,boolean? . ,(msg-proc-1 "boolean?"))
    (,even? . ,(msg-proc-1 "even?"))
    (,integer? . ,(msg-proc-1 "integer?"))
    (,list? . ,(msg-proc-1 "list?"))
    (,negative? . ,(msg-proc-1 "negative?"))
    (,null? . ,(msg-proc-1 "null?"))
    (,number? . ,(msg-proc-1 "number?"))
    (,odd? . ,(msg-proc-1 "odd?"))
    (,pair? . ,(msg-proc-1 "pair?"))
    (,port? . ,(msg-proc-1 "port?"))
    (,positive? . ,(msg-proc-1 "positive?"))
    (,procedure? . ,(msg-proc-1 "procedure?"))
    (,string? . ,(msg-proc-1 "string?"))
    (,symbol? . ,(msg-proc-1 "symbol?"))
    (,zero? . ,(msg-proc-1 "zero?"))))

(define (succeed)
  (set! n-success (+ n-success 1)))

(define (fail proc args)
  (set! n-failure (+ n-failure 1))
  (display* "Failed in " test-name ": Expected ")
  (fail-message proc args))

(define (fail-message proc args)
  (let* ((entry (assq proc fail-message-procs))
         (f (if entry (cdr entry) (msg-proc-1 "??"))))
    (apply f args)
    (newline)))

(define (expect . args)
  (let ((proc (car args))
        (pargs (cdr args)))
    (if (apply proc pargs)
        (succeed)
        (fail proc pargs))))

(define (noexpect . args)
  (letrec ((origproc (car args))
           (pargs (cdr args))
           (proc (lambda a (not (apply origproc a)))))
    (apply expect proc pargs)))

(define (expect-t x)
  (expect eq? #t x))

(define (expect-f x)
  (expect eq? #f x))

(define (test-run)
  (for-each test-run-single (reverse tests))
  (test-summarize)
  (exit (zero? n-failure)))

(define (test-run-single pair)
  (set! test-name (car pair))
  (let ((func (cdr pair)))
    (func)))

(define (test-summarize)
  (display* "Test summary: "
            n-success " succeeded, "
            n-failure " failed, "
            n-ignored " ignored.")
  (newline))

(define (test-init)
  (set! n-failure 0)
  (set! n-success 0))
