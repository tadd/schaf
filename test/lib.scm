;; (define (not v)
;;   (eq? v #f))

;; (define (for-each f l)
;;   (if (not (eq? l '()))
;;       (begin
;;         (f (car l))
;;         (for-each f (cdr l)))))

;; (define (list . a) a)

;; (define (newline)
;;   (display "
;; "))

(define tests '())
(define n-failure 0)
(define n-success 0)
(define n-ignored 0)
(define test-name #f)

(define (describe name f)
  (set! tests (cons (cons name f) tests)))
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

(define fail-message-procs
  (list (cons > (msg-proc-2 ">"))
        (cons equal? (msg-proc-2 "equal?"))
        (cons eq? (msg-proc-2 "eq?"))
        (cons eqv? (msg-proc-2 "eqv?"))
        (cons null? (msg-proc-1 "null?"))))

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
