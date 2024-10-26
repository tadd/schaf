(load "./libtest.scm")

(describe "parsing comments" (lambda ()
  (expect eqv? 1 1 ; foo
               )
  (expect eqv? 2 2 ;;bar
               )
  (expect equal? '(1 2) '(1 ;;; ?? ;;;
                          2))))

(describe "peculiar identifiers" (lambda ()
  (expect eq? '+ '+)
  (expect eq? '- '-)
  (expect eq? '.. '..)
  (expect eq? '... '...)))

;; 4.1. Primitive expression types
;; 4.1.2. Literal expressions
(describe "quote basic" (lambda ()
  (expect equal? '() (list))
  (expect equal? '(1) (list 1))
  (expect equal? '(1 2) (list 1 2))))

(describe "quote" (lambda ()
  (expect eqv? '10 10)
  (expect eq? '() ())
  (expect equal? '(1 2 3) (list 1 2 3))
  (expect eq? 'foooo (quote foooo))
  (let ((l '(+ 1 2)))
     (expect equal? l (quote (+ 1 2)))
     (expect equal? l (list (quote +) 1 2)))
  (expect equal? '(quote a) (list 'quote 'a))
  (expect equal? ''a (list 'quote 'a))
  (expect equal? '"abc" "abc")
  (expect equal? '#t #t)))

;; 4.1.4. Procedures
(describe "lambda" (lambda ()
  (expect procedure? (lambda () 1))
  (expect eqv? ((lambda () 42)) 42)
  (expect eqv? ((lambda (x) (* 2 x)) 21) 42)
  (expect eqv? ((lambda (x y) (* x y)) 3 14) 42)
  (expect eqv? (begin
                 (define mul (lambda (x y) (* x y)))
                 (mul 3 14)) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda () a))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda ()
                    ((lambda () a))))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda (a) a) 10)
                   a) 42)))

(describe "lambda2" (lambda ()
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda () a))
                 (define g (lambda () f))
                 ((g))) 42)
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda () a))
                 (((lambda () f)))) 42)
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda ()
                             (lambda () a)))
                 (define g (f))
                 (g)) 42)
  (expect eqv? (begin
                 (define a 42)
                 (((lambda ()
                     (lambda () a))))) 42)
  (expect eqv? (begin
                 (define a 42)
                 (define f (lambda () a))
                 ((((lambda ()
                      (lambda () f)))))) 42)
  (expect eqv? (((lambda ()
                   42
                   (lambda () 42)))) 42)
  (expect eqv? ((((lambda ()
                    42
                    (lambda ()
                      (lambda () 42)))))) 42)))

(describe "lambda is let" (lambda ()
  (expect eqv? ((lambda (x) x) 42) 42)
  (expect eqv? ((lambda (x y) (+ x y)) 42 21) 63)
  (expect eqv? ((lambda (x)
                  ((lambda (y) (+ x y))
                   21)) 42) 63)
  (expect eqv? ((lambda (x)
                  ((lambda (x) x) 1))
                42) 1)
  (expect eqv? ((lambda (x)
                  ((lambda (y) y)
                   x)) 42) 42)
  (expect eqv? ((lambda (x)
                  ((lambda (x) x)
                   x)) 42) 42)
  (expect eqv? ((lambda (x)
                  ((lambda (x) x) 10)
                  x) 42) 42)
  (expect equal? ((lambda (x)
                    ((lambda (y) `(,x ,y))
                     10)) 42) '(42 10))))

(describe "lambda recursion" (lambda ()
  (expect eqv? (begin
                (define f (lambda (x)
                            (if (> x 0)
                                x
                                (f (+ x 1)))))
                (f 0)) 1)))

(describe "lambda variadic" (lambda ()
  (expect procedure? (lambda x 1))
  (expect eqv? ((lambda x 42)) 42)
  (expect eqv? ((lambda x (* 2 (car x))) 21) 42)
  (expect eqv? ((lambda x (* (car x) (car (cdr x))))
                3 14) 42)
  (expect eqv? (begin
                 (define mul (lambda x (* (car x) (car (cdr x)))))
                 (mul 3 14)) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda x a))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda x ((lambda x a))))) 42)
  (expect eqv? (begin
                 (define a 42)
                 ((lambda a (car a)) 10)
                 a) 42)))

(describe "lambda and envs" (lambda ()
  (define f #f)
  (let ((x 42))
    (set! f (lambda () x)))
  (expect eqv? (f) 42)
  (let ((x 0))
      (expect eqv? (f) 42))))

;; 4.1.5. Conditionals
(describe "if" (lambda ()
  (expect eqv? (if #t 1) 1)
  (expect eqv? (if #t 1 2) 1)
  (expect eqv? (if #f 1 2) 2)))

(describe "if composed" (lambda ()
  (expect eqv? (if (if #t 1 #f)
                   (if #t 3 4)
                   (if #t 5 6)) 3)
  (expect eqv? (if (if #f 1 #f)
                   (if #f 3 4)
                   (if #f 5 6)) 6)))

;; 4.1.6. Assignments
(describe "set!" (lambda ()
  (expect eqv? (begin
                 (define x 1)
                 (set! x 42)
                 x) 42)))

;; 4.2. Derived expression types
;; 4.2.1. Conditionals
(describe "cond" (lambda ()
  (expect eqv? (cond (#f 1)
                     (#t 2)
                     (else 3)) 2)
  (expect eqv? (cond (#f 1)
                     (else 3)) 3)
  (expect eqv? (cond (#f)
                     (2)) 2)
  (expect eq? (cond ((> 3 2) 'greater)
                    ((< 3 2) 'less))
              'greater)
  (expect eq? (cond ((> 3 3) 'greater)
                    ((< 3 3) 'less)
                    (else 'equal))
              'equal)
  (expect eqv? (cond ((assv 'b '((a 1) (b 2))) => cadr)
                    (else #f))
               2)))

(describe "case" (lambda ()
  (expect equal? (case 3
                   ((0 1) 2)
                   ((2 3) 4)
                   (else 5)) 4)
  (expect equal? (case 4
                   ((0 1) 2)
                   ((2 3) 4)
                   (else 5)) 5)
  (expect equal? (case 'c
                   ((a b c) 1)
                   ((d e) 2)
                   (else 3)) 1)
  (expect equal? (case 'f
                   ((a b c) 1)
                   ((d e) 2)
                   (else 3)) 3)

  (expect equal? (case (* 2 3)
                   ((2 3 5 7) 'prime)
                   ((1 4 6 8 9) 'composite))
                 'composite)
  (expect equal? (case (car '(c d))
                   ((a e i o u) 'vowel)
                   ((w y) 'semivowel)
                   (else 'consonant))
                 'consonant)))

(describe "and" (lambda ()
  (expect and)
  (expect and #t)
  (expect eq? (and and) and)
  (expect eqv? (and 1) 1)
  (expect eq? (and 1 "2" 'three) 'three)
  (expect-f (and #f))
  (expect-f (and 1 #f))
  (expect-f (and 1 'two #f))
  (define b #f)
  (define (f) (set! b #t) #f)
  (let* ((x (and #f (f))))
    (expect-f x)
    (expect-f b))))

(describe "or" (lambda ()
  (expect-f (or))
  (expect or #t)
  (expect eq? (or or) or)
  (expect eqv? 1 (or 1))
  (expect eqv? (or 1 "2" 'three) 1)
  (expect-f (or #f))
  (expect-f (or #f #f))
  (expect eqv? (or #f 1) 1)
  (expect eq? (or 'one 2 #f) 'one)
  (define b #t)
  (define (f) (set! b #f) #t)
  (let* ((x (or #t (f))))
    (expect-t x)
    (expect-t b))))

;; 4.2.2. Binding constructs
(describe "let" (lambda ()
  (expect eqv? (let ((x 42)) x) 42)
  (expect eqv? (let ((x 42) (y 21)) (+ x y)) 63)
  (expect eqv? (let ((x 42))
                 (let ((y 21))
                   (+ x y))) 63)
  (expect eqv? (let ((x 42))
                 (let ((x 1))
                   x)) 1)
  (expect eqv? (let ((x 42))
                 (let ((y x))
                   y)) 42)
  (expect eqv? (let ((x 42))
                 (let ((x x))
                   x)) 42)
  (expect eqv? (let ((x 42))
                 (let ((x 10))
                   x)
                 x) 42)
  (expect equal? (let ((x 42) (y 10))
                   `(,x ,y)) '(42 10))))

(describe "let body define" (lambda ()
  (expect eqv? (let ((x 42))
                 (define x 2)
                 x) 2)
  (expect eqv? (begin
                 (define x 1)
                 (let ((x 42))
                   (define x 2)
                   x)) 2)
  (expect eqv? (begin
                 (define x 1)
                 (let ()
                   (define x 2)
                   x)
                 x) 1)))

(describe "named let" (lambda ()
  (expect equal? (let fact () 42) 42)
  (expect equal? (let fact ((n 42)) n) 42)
  (expect equal?
          (let fact ((n 5))
            (if (< n 2)
                n
                (* n (fact (- n 1)))))
          120)
  (expect equal?
          (let loop ((numbers '(3 -2 1 6 -5))
                     (nonneg '())
                     (neg '()))
            (cond ((null? numbers) (list nonneg neg))
                  ((>= (car numbers) 0)
                   (loop (cdr numbers)
                         (cons (car numbers) nonneg)
                         neg))
                  ((< (car numbers) 0)
                   (loop (cdr numbers)
                         nonneg
                         (cons (car numbers) neg)))))
          '((6 1 3) (-5 -2)))))

(describe "let*" (lambda ()
  (expect equal? (let* ((x 42) (y 10))
                   `(,x ,y)) '(42 10))))

(describe "letrec" (lambda ()
  (define retval
    (letrec ((myeven?
              (lambda (n)
                (if (= n 0)
                    #t
                    (myodd? (- n 1)))))
             (myodd?
              (lambda (n)
                (if (= n 0)
                    #f
                    (myeven? (- n 1))))))
      (myeven? 88)))
  (expect-t retval)))

;; 4.2.3. Sequencing
(describe "begin" (lambda ()
  (expect eqv? (begin 1 2 3) 3)))

;; 4.2.4. Iteration
(describe "do" (lambda ()
  (expect equal?
          (do ((l '())
               (i 0 (+ i 1)))
              ((= i 5) l)
            (set! l (append l (list i))))
          '(0 1 2 3 4))
  (expect equal?
          (let ((x '(1 3 5 7 9)))
            (do ((x x (cdr x))
                 (sum 0 (+ sum (car x))))
                ((null? x) sum)))
          25)))

;; 4.2.6. Quasiquotation
(describe "quasiquote basic" (lambda ()
  (expect equal? `() (list))
  (expect equal? `(1) (list 1))

  (expect equal? `(1 2) (list 1 2))
  (expect equal? `() (quasiquote ()))
  (expect equal? `(1) (quasiquote (1)))
  (expect equal? `(1 2) (quasiquote (1 2)))))

(describe "quasiquote" (lambda ()
  (expect eqv? `10 10)
  (expect equal? `"abc" "abc")
  (expect equal? `#t #t)
  (expect eq? `() ())
  (expect equal? `(1 2 3) (list 1 2 3))
  (expect equal? `foooo (quasiquote foooo))
  (let ((l `(+ 1 2)))
     (expect equal? l (quasiquote (+ 1 2)))
     (expect equal? l (list (quasiquote +) 1 2)))))

(describe "quasiquote nested" (lambda ()
  (expect equal? ``1 '(quasiquote 1))
  (expect equal? ``(1) '(quasiquote (1)))
  (expect equal? `(`1) '((quasiquote 1)))
  (expect equal? ```1 '(quasiquote (quasiquote 1)))
  (expect equal? ```(1) '(quasiquote (quasiquote (1))))
  (expect equal? ``(`1) '(quasiquote ((quasiquote 1))))
  (expect equal? `(``1) '((quasiquote (quasiquote 1))))))

(describe "quasiquote unquote" (lambda ()
  (expect eq? `,() ())
  (expect eqv? `,10 10)
  (expect eq? let `,let)
  (expect equal? `(,1 ,2 ,3) (list 1 2 3))
  (let ((x 40))
    (expect equal? `,(+ 2 x) 42)
    (expect equal? `(2 ,x) '(2 40))
    (expect equal? `(1 ,(/ x 2) ,x) '(1 20 40)))
  (expect equal? (let ((a 3))
                   `((1 2) ,a ,4 ,'five 6))
                 `((1 2) 3 4 five 6))
  (expect equal? (quasiquote (list (unquote (+ 1 2)) 4))
                 '(list 3 4))
  (expect equal? '(quasiquote (list (unquote (+ 1 2)) 4))
                 '`(list ,(+ 1 2) 4))))

(describe "quasiquote unquote nested" (lambda ()
  (expect equal? ``,1 '`,1)
  (expect equal? ``,,(+ 1 2) '`,3)

  (expect equal? `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
                 '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
  (expect equal? (let ((name1 'x)
                       (name2 'y))
                   `(a `(b ,,name1 ,',name2 d) e))
                 '(a `(b ,x ,'y d) e))))

(describe "quasiquote unquote-splicing" (lambda ()
  (expect equal? (let ((x '())) `(,@x)) '())
  (expect equal? (let ((x '(42))) `(,@x)) '(42))
  (expect equal? (let ((x '(1 2 3))) `(,@x)) '(1 2 3))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)))
                 '((foo 7)))
  (expect equal? `(1 ,@(cdr '(2)) 3) '(1 3))
  (define (abs x)
    (if (< x 0) (- x) x))
  (expect equal? `(,@(map abs '(4 -5 6)))
                 '(4 5 6))
  (expect equal? `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
                 '(a 3 4 5 6 b))
  (define (sq x)
    (* x x))
  (expect equal? `(10 5 ,(sq 2) ,@(map sq '(4 3)) 8)
                 '(10 5 4 16 9 8))))

(describe "quasiquote improper lists" (lambda ()
  (expect equal? `(1 . 2) '(1 . 2))
  (expect equal? `(1 . ,(car '(2))) '(1 . 2))
  (expect equal? `((foo ,(- 10 3)) ,@(list 8) . ,(car '(9))) '((foo 7) 8 . 9))
  (expect equal? `((foo ,(- 10 3)) ,@(list 8) . ,(car '(cons))) '((foo 7) 8 . cons))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(9))) '((foo 7) . 9))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons))))

;; 5. Program structure
;; 5.2. Definitions
(describe "define variable" (lambda ()
  (expect eqv? (begin
                 (define x 42)
                 x) 42)
  (expect eqv? (begin
                 (define x (* -1 42))
                 x) -42)))

(describe "define function" (lambda ()
  (expect eqv? (begin
                 (define (f) 42)
                 (f)) 42)
  (expect eqv? (begin
                 (define (f x) (* -1 x))
                 (f 42)) -42)))

(describe "define function variadic" (lambda ()
  (expect eqv? (begin
                 (define (f . a) 42)
                 (f)) 42)
  (expect eqv? (begin
                 (define (f . a) (* -1 (car a)))
                 (f 42)) -42)))

(describe "define and lambda" (lambda ()
  (expect eqv? (begin
                 (define f (lambda () (g)))
                 (define g (lambda () 42))
                 (f)) 42)))

;; 6. Standard procedures
;; 6.1. Equivalence predicates
(describe "eqv?" (lambda ()
  (expect eqv? 'a 'a)
  (noexpect eqv? 'a 'b)
  (expect eqv? 2 2)
  (expect eqv? '() '())
  (expect eqv? 100000000 100000000)
  (noexpect eqv? (cons 1 2) (cons 1 2))
  (noexpect eqv? (lambda () 1)
                 (lambda () 2))
  (noexpect eqv? #f 'nil)
  (let ((p (lambda (x) x)))
    (expect eqv? p p))

  (expect eqv? #t #t)
  (expect eqv? #f #f)
  (let ((s "foo"))
    (expect eqv? s s))
  (expect eqv? eqv? eqv?)
  (noexpect eqv? #t #f)
  (noexpect eqv? #f #t)
  (noexpect eqv? 1 2)
  (noexpect eqv? '() '(1))
  (noexpect eqv? "a" "a")
  (noexpect eqv? #f 'nil)))

(describe "eqv? complicated" (lambda ()
  (define gen-counter
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) n))))
  (let ((g (gen-counter)))
    (expect eqv? g g))
  (noexpect eqv? (gen-counter) (gen-counter))

  (define gen-loser
    (lambda ()
      (let ((n 0))
        (lambda () (set! n (+ n 1)) 27))))
  (let ((g (gen-loser)))
    (expect eqv? g g))

  (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
           (g (lambda () (if (eqv? f g) 'g 'both))))
    (noexpect eqv? f g))))

(describe "eq?" (lambda ()
  (expect eq? 'a 'a)
  (noexpect eq? (list 'a) (list 'a))
  (expect eq? '() '())
  (expect eq? car car)
  (let ((x '(a)))
    (expect eq? x x))
  (let ((x '()))
    (expect eq? x x))
  (let ((p (lambda (x) x)))
    (expect eq? p p))

  (expect eq? #t #t)
  (expect eq? #f #f)
  (noexpect eq? #t #f)
  (noexpect eq? () '(1))
  (noexpect eq? '(1) '(1))
  (noexpect eq? '(1 '(2)) '(1 '(2)))))

(describe "equal?" (lambda ()
  (expect equal? 'a 'a)
  (expect equal? '(a) '(a))
  (expect equal? '(a (b) c) '(a (b) c))
  (expect equal? "abc" "abc")
  (expect equal? 2 2)

  (expect equal? #t #t)
  (expect equal? #f #f)
  (noexpect equal? #t #f)
  (noexpect equal? 1 -1)
  (expect equal? () ())
  (noexpect equal? () '(1))
  (expect-t (let ((x '(1)))
              (equal? x x)))
  (expect-t (let ((p (lambda (x) x)))
              (equal? p p)))
  (expect equal? "abc" "abc")
  (expect equal? "\"" "\"")
  (noexpect equal? "abc" "abd")
  (noexpect equal? "abc\"" "\"")))

;; 6.2. Numbers
;; 6.2.5. Numerical operations
(describe "number?" (lambda ()
  (expect number? 0)
  (expect number? 1)
  (expect number? 100000)
  (expect number? -0)
  (expect number? -1)
  (expect number? -1000000)

  (noexpect number? 'a)
  (noexpect number? '())
  (noexpect number? '(1))
  (noexpect number? "1")
  (noexpect number? #t)
  (noexpect number? #f)
  (noexpect number? number?)))

(describe "integer?" (lambda ()
  (expect integer? 0)
  (expect integer? 1)
  (expect integer? 100000)
  (expect integer? -0)
  (expect integer? -1)
  (expect integer? -1000000)

  (noexpect integer? 'a)
  (noexpect integer? '())
  (noexpect integer? '(1))
  (noexpect integer? "1")
  (noexpect integer? #t)
  (noexpect integer? #f)
  (noexpect integer? integer?)))

(describe "=" (lambda ()
  (expect = 42 42)
  (expect = 0 0 0 0 0)

  (noexpect = 42 0)
  (noexpect = 0 0 0 0 42)))

(describe "<" (lambda ()
  (expect < 2 4)
  (expect < 2 3 4 5)
  (noexpect < 2 0)
  (noexpect < 2 3 4 4)))

(describe ">" (lambda ()
  (expect > 3 2)
  (expect > 4 3 2 1)
  (noexpect > 0 1)
  (noexpect > 4 3 2 2)))

(describe "<=" (lambda ()
  (expect <= 2 4)
  (expect <= 2 3 4 4)
  (noexpect <= 2 0)
  (noexpect <= 2 3 4 3)))

(describe ">=" (lambda ()
  (expect >= 3 2)
  (expect >= 4 3 2 2)
  (noexpect >= 0 1)
  (noexpect >= 4 3 2 3)))

(describe "zero?" (lambda ()
  (expect zero? 0)
  (expect zero? -0)

  (noexpect zero? 1)
  (noexpect zero? -1)
  (noexpect zero? 99999)
  (noexpect zero? -99999)

  (noexpect zero? 'a)
  (noexpect zero? '())
  (noexpect zero? '(1))
  (noexpect zero? "1")
  (noexpect zero? #t)
  (noexpect zero? #f)
  (noexpect zero? zero?)))

(describe "positive?" (lambda ()
  (expect positive? 1)
  (expect positive? 1000000)

  (noexpect positive? 0)
  (noexpect positive? -1)
  (noexpect positive? -99999)

  (noexpect positive? 'a)
  (noexpect positive? '())
  (noexpect positive? '(1))
  (noexpect positive? "1")
  (noexpect positive? #t)
  (noexpect positive? #f)
  (noexpect positive? positive?)))

(describe "negative?" (lambda ()
  (expect negative? -1)
  (expect negative? -1000000)

  (noexpect negative? 0)
  (noexpect negative? 1)
  (noexpect negative? 1000000)

  (noexpect negative? 'a)
  (noexpect negative? '())
  (noexpect negative? '(1))
  (noexpect negative? "1")
  (noexpect negative? #t)
  (noexpect negative? #f)
  (noexpect negative? negative?)))

(describe "odd?" (lambda ()
  (expect odd? 1)
  (expect odd? 3)
  (expect odd? -1)
  (expect odd? 100001)
  (expect odd? -1)
  (expect odd? -1000001)

  (noexpect odd? 0)
  (noexpect odd? 2)
  (noexpect odd? -0)
  (noexpect odd? 100000)
  (noexpect odd? -2)
  (noexpect odd? -100000)

  (noexpect odd? 'a)
  (noexpect odd? '())
  (noexpect odd? '(1))
  (noexpect odd? "1")
  (noexpect odd? #t)
  (noexpect odd? #f)
  (noexpect odd? odd?)))

(describe "even?" (lambda ()
  (expect even? 0)
  (expect even? 2)
  (expect even? -2)
  (expect even? 100000)
  (expect even? -0)
  (expect even? -1000000)

  (noexpect even? 1)
  (noexpect even? 3)
  (noexpect even? -3)
  (noexpect even? 99999)
  (noexpect even? -1)
  (noexpect even? -99999)

  (noexpect even? 'a)
  (noexpect even? '())
  (noexpect even? '(1))
  (noexpect even? "1")
  (noexpect even? #t)
  (noexpect even? #f)
  (noexpect even? even?)))

(describe "max" (lambda ()
  (expect equal? (max 0) 0)
  (expect equal? (max 0 2 1) 2)
  (expect equal? (max 0 0 0) 0)
  (expect equal? (max -10) -10)
  (expect equal? (max -10 -20 -30) -10)
  (expect equal? (max -1 -1 -1) -1)))

(describe "min" (lambda ()
  (expect equal? (min 0) 0)
  (expect equal? (min 0 2 1) 0)
  (expect equal? (min 2 2 2) 2)
  (expect equal? (min -10) -10)
  (expect equal? (min -10 -20 -30) -30)
  (expect equal? (min -1 -1 -1) -1)))

(describe "+" (lambda ()
  (expect eqv? (+ 42 21) 63)))

(describe "-" (lambda ()
  (expect eqv? (- 42 21) 21)))

(describe "*" (lambda ()
  (expect eqv? (* 4 2) 8)))

(describe "/" (lambda ()
  (expect eqv? (/ 4 2) 2)))

(describe "abs" (lambda ()
  (expect equal? (abs 0) 0)
  (expect equal? (abs 1) 1)
  (expect equal? (abs -1) 1)
  (expect equal? (abs -12345678) 12345678)))

(describe "arithmetic" (lambda ()
  (expect eqv? (+ (+ 40 2) 21) 63)
  (expect eqv? (+ (- 40 4) (* 3 (/ 100 50))) 42)))

(describe "quotient" (lambda ()
  (expect eqv? (quotient 1 1) 1)
  (expect eqv? (quotient 0 1) 0)
  (expect eqv? (quotient 0 4) 0)
  (expect eqv? (quotient 8 4) 2)
  (expect eqv? (quotient 12 4) 3)
  (expect eqv? (quotient -12 4) -3)
  (expect eqv? (quotient 12 -4) -3)
  (expect eqv? (quotient -12 -4) 3)))

(describe "remainder" (lambda ()
  (expect eqv? (remainder 13 4) 1)
  (expect eqv? (remainder -13 4) -1)
  (expect eqv? (remainder 13 -4) 1)
  (expect eqv? (remainder -13 -4) -1)))

(describe "modulo" (lambda ()
  (expect eqv? (modulo 13 4) 1)
  (expect eqv? (modulo -13 4) 3)
  (expect eqv? (modulo 13 -4) -3)
  (expect eqv? (modulo -13 -4) -1)))

(describe "expt" (lambda ()
  (expect eqv? (expt 1 1) 1)
  (expect eqv? (expt 2 2) 4)
  (expect eqv? (expt 2 3) 8)
  (expect eqv? (expt 2 8) 256)
  (expect eqv? (expt -2 2) 4)
  (expect eqv? (expt 1 0) 1)
  (expect eqv? (expt -1 0) 1)
  (expect eqv? (expt 0 1) 0)
  (expect eqv? (expt 0 0) 1)))

;; 6.3. Other data types
;; 6.3.1. Booleans
(describe "true/false" (lambda ()
  (expect-t #t)
  (expect-f #f)))

(describe "not" (lambda ()
  (expect not #f)
  (expect not (not #t))
  (expect not (not (not #f)))))

(describe "boolean?" (lambda ()
  (expect boolean? #t)
  (expect boolean? #f)

  (noexpect boolean? 0)
  (noexpect boolean? '(#t))
  (noexpect boolean? "#t")
  (noexpect boolean? boolean?)))

;; 6.3.2. Pairs and lists
(describe "pair?" (lambda ()
  (expect pair? '(1))
  (expect pair? '(1 . 2))
  (noexpect pair? '())
  (noexpect pair? 1)
  (noexpect pair? 'a)
  (noexpect pair? #f)))

(describe "cons" (lambda ()
  (expect equal? '(1 . 2) (cons 1 2))))

(describe "car" (lambda ()
  (expect eqv? (car '(1 . 2)) 1)))

(describe "cdr" (lambda ()
  (expect eqv? (cdr '(1 . 2)) 2)))

(describe "cxxr" (lambda ()
  (define l '(1 2 3 4))
  (expect equal? (cadr l) 2)
  (expect equal? (caddr l) 3)
  (expect equal? (cadddr l) 4)))

(describe "null?" (lambda ()
  (expect null? ())
  (expect null? (list))
  (noexpect null? '(1))
  (noexpect null? 1)))

(describe "list?" (lambda ()
  (expect list? '(a b c))
  (expect list? '())
  (noexpect list? '(a . b))
  (noexpect list? 1)
  (let ((x (list 'a)))
    ;; (set-cdr! x x)
    (expect list? x))))

(describe "list" (lambda ()
  (expect null? '())
  (expect null? (list))
  (let ((l '(42 "foo")))
    (expect pair? l)
    (expect eqv? (length l) 2)
    (expect eqv? (car l) 42)
    (expect equal? (car (cdr l)) "foo"))))

(describe "length" (lambda ()
  (expect eqv? (length '()) 0)
  (expect eqv? (length '(1)) 1)
  (expect eqv? (length '(1 2 3 4)) 4)))

(describe "append" (lambda ()
  (expect null? (append))
  (expect equal? (append '(1))
          '(1))
  (expect equal? (append '(1) '(2))
          '(1 2))
  (expect equal? (append '(1) '(2 3))
          '(1 2 3))
  (expect equal? (append '(1 '(2)) '('(3)))
          '(1 '(2) '(3)))
  (expect equal? (append '(1 2) '(3 . 4))
          '(1 . (2 . (3 . 4))))
  (expect equal? (append '(1) '(2) '(3))
          '(1 2 3))))

(describe "reverse" (lambda ()
  (expect equal? () ())
  (expect equal? '(1) (reverse '(1)))
  (expect equal? '(2 1) (reverse '(1 2)))
  (expect equal? '(3 2 1) (reverse '(1 2 3)))))

(describe "list-tail" (lambda ()
  (expect equal? (list-tail '(a b c d) 0) '(a b c d))
  (expect equal? (list-tail '(a b c d) 2) '(c d))
  (expect equal? (list-tail '(a b c d) 3) '(d))))

(describe "list-ref" (lambda ()
  (expect equal? (list-ref '(a b c d) 0) 'a)
  (expect equal? (list-ref '(a b c d) 2) 'c)
  (expect equal? (list-ref '(a b c d) 3) 'd)))

(describe "memq" (lambda ()
  (expect equal? (memq 'a '(a b c)) '(a b c))
  (expect equal? (memq 'b '(a b c)) '(b c))
  (expect equal? (memq 'a '(b c d)) #f)))

(describe "memv" (lambda ()
  (expect equal? (memv 'a '(a b c)) '(a b c))
  (expect equal? (memv 'b '(a b c)) '(b c))
  (expect equal? (memv 'a '(b c d)) #f)
  (expect equal? (memv 101 '(100 101 102)) '(101 102))))

(describe "member" (lambda ()
  (expect equal? (member 'a '(a b c)) '(a b c))
  (expect equal? (member 'b '(a b c)) '(b c))
  (expect equal? (member 'a '(b c d)) #f)
  (expect equal? (member 101 '(100 101 102)) '(101 102))
  (expect equal? (member (list 'a) '(b (a) c)) '((a) c))))

(describe "assq" (lambda ()
  (define e '((a 1) (b 2) (c 3)))
  (expect equal? (assq 'a e)
                 '(a 1))
  (expect equal? (assq 'b e)
                 '(b 2))
  (expect equal? (assq 'd e)
                 #f)
  (expect equal? (assq (list 'a) '(((a)) ((b)) ((c))))
                 #f)))

(describe "assv" (lambda ()
  (define e '((a 1) (b 2) (c 3)))
  (expect equal? (assv 'a e)
                 '(a 1))
  (expect equal? (assv 'b e)
                 '(b 2))
  (expect equal? (assv 'd e)
                 #f)
  (expect equal? (assv (list 'a) '(((a)) ((b)) ((c))))
                 #f)
  (expect equal? (assv 5 '((2 3) (5 7) (11 13)))
                 '(5 7))))

(describe "assoc" (lambda ()
  (define e '((a 1) (b 2) (c 3)))
  (expect equal? (assoc 'a e)
                 '(a 1))
  (expect equal? (assoc 'b e)
                 '(b 2))
  (expect equal? (assoc 'd e)
                 #f)
  (expect equal? (assoc (list 'a) '(((a)) ((b)) ((c))))
                 '((a)))
  (expect equal? (assoc 5 '((2 3) (5 7) (11 13)))
                 '(5 7))))

;; 6.3.3. Symbols
(describe "symbol?" (lambda ()
  (expect symbol? 'foo)
  (expect symbol? (car '(a b)))
  (expect symbol? 'nil)
  (noexpect symbol? "bar")
  (noexpect symbol? '())
  (noexpect symbol? #f)))

;; 6.3.5. Strings
(describe "string?" (lambda ()
  (expect string? "foo")
  (expect string? "")
  (expect string? "\"bar\"")
  (noexpect string? 'bar)
  (noexpect string? '())
  (noexpect string? 10)
  (noexpect string? #f)
  (noexpect string? string?)))

(describe "string-length" (lambda ()
  (expect equal? (string-length "foo") 3)
  (expect equal? (string-length "o") 1)
  (expect equal? (string-length "") 0)))

(describe "string=?" (lambda ()
  (expect string=? "foo" "foo")
  (expect string=? "" "")
  (expect string=? " " " ")
  (expect string=? "\\" "\\")
  (expect string=? "a\"b" "a\"b")
  ;; newlines
  (expect string=? "
" "
")
  (noexpect string=? "" "
")
  (noexpect string=? "a" "ab")
  (noexpect string=? "\\" "\\\\")
  (noexpect string=? "ab" "a\"b")))

;; 6.4. Control features
(describe "procedure?" (lambda ()
  (expect procedure? car)
  (noexpect procedure? 'car)
  (expect procedure? (lambda (x) (* x x)))
  (noexpect procedure? '(lambda (x) (* x x)))
  (expect-t (call/cc (lambda (c) (procedure? c))))))

(describe "apply" (lambda ()
  (expect equal? (apply + '(42)) 42)
  (expect equal? (apply + 1 '(42)) 43)
  (expect equal? (apply + 1 2 '(42)) 45)
  (expect equal? (apply + 1 2 3 '(42)) 48)
  (expect equal? (apply + '(1 2 3 42)) 48)
  (expect equal? (apply + (apply + '(1 2 3)) '(42)) 48)
  (expect-t (apply equal? '((1) (1))))))

(describe "apply variadic" (lambda ()
  (expect-t (apply = 1 1 1 1 1 '(1 1 1 1 1)))))

(describe "map" (lambda ()
  (expect equal? (map car '((a b) (d e) (g h))) '(a d g))
  (expect equal? (map (lambda (n) (* n n)) '(1 2 3 4 5)) '(1 4 9 16 25))
  (expect equal? (map + '(1 2 3) '(4 5 6)) '(5 7 9))
  (expect equal? (map + '(1 2) '(4)) '(5))))

(describe "for-each" (lambda ()
  (let ((x '()))
    (for-each (lambda (l) (set! x (car l))) '((a b) (d e) (g h)))
    (expect eq? x 'g))
  (let ((x 0))
    (for-each (lambda (n) (set! x (* n n))) '(1 2 3 4 5))
    (expect eqv? x 25))
  (let ((x 0))
    (for-each (lambda (a b) (set! x (+ a b))) '(1 2 3) '(4 5 6))
    (expect eqv? x 9))
  (let ((x 0))
    (for-each (lambda (a b) (set! x (+ a b))) '(1 2) '(4))
    (expect eqv? x 5))))

(describe "call/cc applicable in call/cc" (lambda ()
  (define (f)
    (call/cc call/cc)
    42)
  (expect eqv? (f) 42)))

;; Above call/cc tests were from Kawa's test suite under the MIT license

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
; I believe the result is unspecified
;; (describe "call/cc and lambda" (lambda ()
;;   (expect eqv? (call/cc (lambda (c) (0 (c 1)))) 1)))

(describe "call/cc retlec" (lambda ()
  (define (f)
    (letrec ((x (call/cc list))
             (y (call/cc list)))
      (cond ((procedure? x) (x (pair? y)))
	    ((procedure? y) (y (pair? x))))
      (let ((x (car x))
            (y (car y)))
        (and (call/cc x) (call/cc y) (call/cc x)))))
  (expect-t (f))))

(describe "call/cc in-yo" (lambda ()
  (define r
    (let ((x ())
          (y 0)
          (id (lambda (x) x)))
      (call/cc
       (lambda (escape)
         (let* ((in ((lambda (foo)
                       (set! x (cons y x))
                       (if (= y 10)
                           (escape x)
                           (begin
                             (set! y 0)
                             foo)))
                     (call/cc id)))
                (yo ((lambda (foo)
                       (set! y (+ y 1))
                       foo)
                     (call/cc id))))
           (in yo))))))
  (expect equal? r '(10 9 8 7 6 5 4 3 2 1 0))))

(describe "call/cc each other" (lambda ()
  (define r #f)
  (define a #f)
  (define b #f)
  (define c #f)
  (define i 0)
  (define (f1)
    (let ()
      (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
                 (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
      (if (not c)
          (set! c a))
      (set! i (+ i 1))
      (case i
        ((1) (a 5))
        ((2) (b 8))
        ((3) (a 6))
        ((4) (c 4)))
      r))
  (define (f2)
    (let ()
      (set! r (+ 1 (+ 2 (+ 3 (call/cc (lambda (k) (set! a k) 4))))
                 (+ 5 (+ 6 (call/cc (lambda (k) (set! b k) 7))))))
      (if (not c)
          (set! c a))
      (set! i (+ i 1))
      (case i
        ((1) (b 8))
        ((2) (a 5))
        ((3) (b 7))
        ((4) (c 4)))
      r))
  (expect eqv? (f1) 28)
  (expect eqv? (f2) 28)))

(describe "call/cc lazy callframe" (lambda ()
  (define (f)
    (let ((k1 #f)
          (k2 #f)
          (k3 #f)
          (state 0))
      (define (identity x) x)
      (define (fn)
        ((identity (if (= state 0)
                       (call/cc (lambda (k) (set! k1 k) +))
                       +))
         (identity (if (= state 0)
                       (call/cc (lambda (k) (set! k2 k) 1))
                       1))
         (identity (if (= state 0)
                       (call/cc (lambda (k) (set! k3 k) 2))
                       2))))
      (define (check states)
        (set! state 0)
        (let* ((res '())
               (r (fn)))
          (set! res (cons r res))
          (if (null? states)
              res
              (begin
                (set! state (car states))
                (set! states (cdr states))
                (case state
                  ((1) (k3 4))
                  ((2) (k2 2))
                  ((3) (k1 -)))))))
      (map check '((1 2 3)
                   (1 3 2)
                   (2 1 3)
                   (2 3 1)
                   (3 1 2)
                   (3 2 1)))))
  (expect equal? (f) '((-1 4 5 3)
                       (4 -1 5 3)
                       (-1 5 4 3)
                       (5 -1 4 3)
                       (4 5 -1 3)
                       (5 4 -1 3)))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/unreach1.scm
(describe "call/cc unreached 1" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let l ()
         (return #f)
         (l))))
    #t)
  (expect-t (f))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/unreach2.scm
(describe "call/cc unreached 2" (lambda ()
  (define (f)
    (call/cc
     (lambda (return)
       (let ((a 1))
         (let loop ((a a))
           (let ((finish (lambda (a) (return #f))))
             (finish a)
             (let ((a 2))
               (loop a))))))))
  (expect-f (f))))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/sva35362.scm
(describe "call/cc unused" (lambda ()
  (define (f) ;; never called
    (call/cc
     (lambda (return)
       (let l ()
         (l)))))
  (expect-t #t)))

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/sva40649.scm
(describe "call/cc NPE" (lambda ()
  (define (f1 f2) (f2))

  (define (fa x)
    (call/cc
     (lambda (k)
       (define (f3) x)
       (f1 f3))))

  (define (fb x)
    (call/cc
     (lambda (k)
       (define (f3) x)
       (f1 f3)
       (+ 10 x))))

  (define (fc x)
    (call/cc
     (lambda (k)
       (define (f3) x)
       (f1 f3)
       (if (> x 0)
           (k (+ 20 x)))
       (+ 10 x))))

  (expect eqv? (fa 3) 3)
  (expect eqv? (fb 3) 13)
  (expect eqv? (fc 3) 23)))
;; End of tests from Kawa

;; Local Extensions
(describe "cputime" (lambda ()
  (let ((t (_cputime)))
    (expect number? t)
    (expect > t 0))))

;; (load "./test-callcc.scm")

(test-run)
