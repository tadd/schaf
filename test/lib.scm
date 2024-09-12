(define (not v)
  (eq? v #f))

(define (null? l)
  (eq? l '()))

(define (begin . a)
  '())

(define (for-each f l)
  (if (not (null? l))
      (begin
        (f (car l))
        (for-each f (cdr l)))))

(define (list . a) a)

(define (newline)
  (display "
"))

;; (define (reverse l)
;;   (if (null? l)
;;       l
;;       (if (null? (cdr l))
;;           (list (car l))
;;           (apply list (reverse (cdr l)) (car l)))))

(define eqv? eq?)

(define (equal? x y)
  (if (eq? x y)
      #t
      (if (and (pair? x) (pair? y))
          (and (equal? (car x) (car y))
               (equal? (cdr x) (cdr y)))
          #f)))

(define = eq?)

(define (caar o)
  (car (car o)))

(define (assq o alist)
  (if (null? alist)
      #f
      (if (eq? o (caar alist))
          (car alist)
          (assq o (cdr alist)))))

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

(define (succeed)
  ())

(define (fail proc args)
  (display* "Failed in " test-name ": Expected ")
  (fail-message proc args))

(define (fail-message proc args)
  (apply display* "OK with" args)
  (newline))

(define (expect . args)
  (let ((proc (car args))
        (pargs (cdr args)))
    (if (apply proc pargs)
        (succeed)
        (fail proc pargs))))

(define (noexpect . args)
  (let ((origproc (car args))
        (pargs (cdr args)))
    (let (proc (lambda a (not (apply origproc a))))
      (apply expect proc pargs))))

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
