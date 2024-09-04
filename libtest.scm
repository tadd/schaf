(define tests '())
(define n-failure 0)
(define n-success 0)
(define test-name '())

(define (describe name f)
  (set! tests `((,name . ,f) . ,tests)))
;;(define context describe)

(define (display* . args)
  (for-each display args))

(define (msg-proc-1 msg)
  (lambda (x)
    (display* "<" x "> to " msg)))

(define (msg-proc-2 msg)
  (lambda (x y)
    ((msg-proc-1 msg) x)
    (display* " <" y ">")))

(define fail-message-procs
  `((,> . ,(msg-proc-2 "be >"))
    (,equal? . ,(msg-proc-2 "be equal?"))
    (,eq? . ,(msg-proc-2 "be eq?"))
    (,null? . ,(msg-proc-1 "be null?"))))

(define (succeed)
  (set! n-success (+ n-success 1)))

(define (fail proc args)
  (set! n-failure (+ n-failure 1))
  (display* "Failed in " test-name ": Expected ")
  (fail-message proc args)
  (newline))

(define (fail-message proc args)
  (let ((f (assq proc fail-message-procs)))
    (apply (cdr f) args)))

(define (expect . args)
  (let ((proc (car args))
        (pargs (cdr args)))
    (if (apply proc pargs)
        (succeed)
        (fail proc pargs))))

(define (expect-t x)
  (expect eq? #t x))

(define (expect-f x)
  (expect eq? #f x))

(define (test-run)
  (for-each test-run-single (reverse tests))
  (test-summarize))

(define (test-run-single pair)
  (let ((name (car pair))
        (func (cdr pair)))
    (set! test-name name)
    (func)))

(define (test-summarize)
  (display* "Test summary: "
            n-success " succeeded, "
            n-failure " failed.")
  (newline))
