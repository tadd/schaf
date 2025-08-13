(load "./lib.scm")

(define r7rs? #t)             ;; tweak me when to use other pure-R5RS implementations
(define local? (and r7rs? #t));; tweak me when to use any other implementations

(describe "parsing comments" (lambda ()
  (expect = 1 1 ; foo
               )
  (expect = 2 2 ;;bar
               )
  (expect equal? '(1 2) '(1 ;;; ?? ;;;
                          2))))

(describe "peculiar identifiers" (lambda ()
  (expect eq? '+ '+)
  (expect eq? '- '-)
  (expect eq? '.. '..)
  (expect eq? '... '...)))

;; 4. Expressions
;; 4.1. Primitive expression types
;; 4.1.2. Literal expressions
(describe "quote basic" (lambda ()
  (expect equal? '() (list))
  (expect equal? '(1) (list 1))
  (expect equal? '(1 2) (list 1 2))))

(describe "quote" (lambda ()
  (expect = '10 10)
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
  (expect = ((lambda () 42)) 42)
  (expect = ((lambda (x) (* 2 x)) 21) 42)
  (expect = ((lambda (x y) (* x y)) 3 14) 42)
  (let ((mul (lambda (x y) (* x y))))
    (expect = (mul 3 14) 42))
  (let ((a 42))
    (expect = ((lambda () a)) 42))
  (let ((a 42))
    (expect = ((lambda ()
                 ((lambda () a)))) 42))
  (expect = (let ((a 42))
              ((lambda (a) a) 10)
              a) 42)))

(describe "lambda2" (lambda ()
  (let ((a 42))
    (define f (lambda () a))
    (define g (lambda () f))
    (expect = ((g)) 42))
  (let ((a 42))
    (define f (lambda () a))
    (expect =  (((lambda () f))) 42))
  (let ((a 42))
    (define f (lambda ()
                (lambda () a)))
    (define g (f))
    (expect = (g) 42))
  (let ((a 42))
    (expect = (((lambda ()
      (lambda () a)))) 42))
  (let ((a 42))
    (define f (lambda () a))
    (expect = ((((lambda ()
       (lambda () f))))) 42))
  (expect = (((lambda ()
                42
                (lambda () 42)))) 42)
  (expect = ((((lambda ()
                 42
                 (lambda ()
                   (lambda () 42)))))) 42)))

(describe "lambda is let" (lambda ()
  (expect = ((lambda (x) x) 42) 42)
  (expect = ((lambda (x y) (+ x y)) 42 21) 63)
  (expect = ((lambda (x)
               ((lambda (y) (+ x y))
                21)) 42) 63)
  (expect = ((lambda (x)
               ((lambda (x) x) 1))
             42) 1)
  (expect = ((lambda (x)
               ((lambda (y) y)
                x)) 42) 42)
  (expect = ((lambda (x)
               ((lambda (x) x)
                x)) 42) 42)
  (expect = ((lambda (x)
               ((lambda (x) x) 10)
               x) 42) 42)
  (expect equal? ((lambda (x)
                    ((lambda (y) `(,x ,y))
                     10)) 42) '(42 10))))

(describe "lambda recursion" (lambda ()
  (letrec ((f (lambda (x)
                (if (> x 0)
                    x
                    (f (+ x 1))))))
    (expect = (f 0) 1))))

(describe "lambda variadic" (lambda ()
  (expect procedure? (lambda x 1))
  (expect = ((lambda x 42)) 42)
  (expect = ((lambda x (* 2 (car x))) 21) 42)
  (expect = ((lambda x (* (car x) (car (cdr x))))
             3 14) 42)
  (let ((mul (lambda x (* (car x) (car (cdr x))))))
    (expect = (mul 3 14) 42))
  (let ((a 42))
    (expect = ((lambda x a)) 42))
  (let ((a 42))
    (expect = ((lambda x ((lambda x a)))) 42))
  (expect = (let ((a 42))
              ((lambda a (car a)) 10)
              a) 42)))

(describe "lambda and envs" (lambda ()
  (define f #f)
  (let ((x 42))
    (set! f (lambda () x)))
  (expect = (f) 42)
  (let ((x 0))
      (expect = (f) 42))))

(describe "lambda and envs 2" (lambda ()
  (let ((x #f))
    (let* ((f (lambda () x))
           (x #t))
      (expect-f (f)))
    (let* ((x #t)
           (f (lambda () x)))
    (expect-t (f)))
    (let* ((x #t)
           (f (lambda () x)))
      (expect-t (f))))))

;; 4.1.5. Conditionals
(describe "if" (lambda ()
  (expect = (if #t 1) 1)
  (expect = (if #t 1 2) 1)
  (expect = (if #f 1 2) 2)))

(describe "if composed" (lambda ()
  (expect = (if (if #t 1 #f)
                (if #t 3 4)
                (if #t 5 6)) 3)
  (expect = (if (if #f 1 #f)
                (if #f 3 4)
                (if #f 5 6)) 6)))

;; 4.1.6. Assignments
(describe "set!" (lambda ()
  (let ((x 1))
    (set! x 42)
    (expect = x 42))))

;; 4.2. Derived expression types
;; 4.2.1. Conditionals
(describe "cond" (lambda ()
  (expect = (cond (#f 1)
                  (#t 2)
                  (else 3)) 2)
  (expect = (cond (#f 1)
                  (else 3)) 3)
  (expect = (cond (#f)
                  (2)) 2)
  (expect eq? (cond ((> 3 2) 'greater)
                    ((< 3 2) 'less))
              'greater)
  (expect eq? (cond ((> 3 3) 'greater)
                    ((< 3 3) 'less)
                    (else 'equal))
              'equal)
  (expect = (cond ((assv 'b '((a 1) (b 2))) => cadr)
                  (else #f))
            2)))

(describe "case" (lambda ()
  (expect = (case 3
              ((0 1) 2)
              ((2 3) 4)
              (else 5)) 4)
  (expect = (case 4
              ((0 1) 2)
              ((2 3) 4)
              (else 5)) 5)
  (expect = (case 'c
              ((a b c) 1)
              ((d e) 2)
              (else 3)) 1)
  (expect = (case 'f
              ((a b c) 1)
              ((d e) 2)
              (else 3)) 3)

  (expect eq? (case (* 2 3)
                ((2 3 5 7) 'prime)
                ((1 4 6 8 9) 'composite))
          'composite)
  (expect eq? (case (car '(c d))
                ((a e i o u) 'vowel)
                ((w y) 'semivowel)
                (else 'consonant))
          'consonant)))

(describe "and" (lambda ()
  (define b #f)
  (define (f) (set! b #t) #f)
  (expect-t (and))
  (expect eq? (and eq?) eq?)
  (expect = (and 1) 1)
  (expect eq? (and 1 "2" 'three) 'three)
  (expect-f (and #f))
  (expect-f (and 1 #f))
  (expect-f (and 1 'two #f))
  (let* ((x (and #f (f))))
    (expect-f x)
    (expect-f b))))

(describe "or" (lambda ()
  (define b #t)
  (define (f) (set! b #f) #t)
  (expect-f (or))
  (expect eq? (or eq?) eq?)
  (expect = 1 (or 1))
  (expect = (or 1 "2" 'three) 1)
  (expect-f (or #f))
  (expect-f (or #f #f))
  (expect = (or #f 1) 1)
  (expect eq? (or 'one 2 #f) 'one)
  (let* ((x (or #t (f))))
    (expect-t x)
    (expect-t b))))

;; 4.2.2. Binding constructs
(describe "let" (lambda ()
  (expect = (let ((x 42)) x) 42)
  (let ((x 42))
    (expect = x 42))
  (let ((x 42) (y 21)) (expect = (+ x y) 63))
  (let ((x 42))
    (let ((y 21))
      (expect = (+ x y) 63)))
  (let ((x 42))
    (let ((x 1))
      (expect = x 1)))
  (let ((x 42))
    (let ((y x))
      (expect = y 42)))
  (let ((x 42))
    (let ((x x))
      (expect = x 42)))
  (expect = (let ((x 42))
              (let ((x 10))
                x)
              x) 42)
  (let ((x 42) (y 10))
    (expect equal? `(,x ,y) '(42 10)))))

(describe "let body define" (lambda ()
  (let ((x 42))
    (define x 2)
    (expect = x 2))
  (let ((x 1))
    (let ((x 42))
      (define x 2)
      (expect = x 2)))
  (expect = (let ((x 1))
              (let ()
                (define x 2)
                x)
              x) 1)))

(describe "named let" (lambda ()
  (expect = (let fact () 42) 42)
  (expect = (let fact ((n 42)) n) 42)
  (expect = (let fact ((n 5))
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
  (expect = 42 (let* () 42)) ;; accepts () for bindings
  (let* ((x 42) (y 10))
    (expect equal? `(,x ,y) '(42 10)))))

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
  (expect = 42 (letrec () 42)) ;; accepts () for bindings
  (expect-t retval)))

;; 4.2.3. Sequencing
(describe "begin" (lambda ()
  (expect = (begin 1 2 3) 3)))

;; 4.2.4. Iteration
(describe "do" (lambda ()
  (expect = 42 (do () (#t 42))) ;; accepts () for bindings
  (expect equal?
          (do ((l '())
               (i 0 (+ i 1)))
              ((= i 5) l)
            (set! l (append l (list i))))
          '(0 1 2 3 4))
  (let ((x '(1 3 5 7 9)))
    (expect =
            (do ((x x (cdr x))
                 (sum 0 (+ sum (car x))))
                ((null? x) sum))
            25))))

;; 4.2.6. Quasiquotation
(describe "quasiquote basic" (lambda ()
  (expect equal? `() (list))
  (expect equal? `(1) (list 1))

  (expect equal? `(1 2) (list 1 2))
  (expect equal? `() (quasiquote ()))
  (expect equal? `(1) (quasiquote (1)))
  (expect equal? `(1 2) (quasiquote (1 2)))))

(describe "quasiquote" (lambda ()
  (expect = `10 10)
  (expect equal? `"abc" "abc")
  (expect eq? `#t #t)
  (expect eq? `() '())
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
  (expect eq? `,() '())
  (expect = `,10 10)
  (expect eq? let `,let)
  (expect equal? `(,1 ,2 ,3) (list 1 2 3))
  (let ((x 40))
    (expect = `,(+ 2 x) 42)
    (expect equal? `(2 ,x) '(2 40))
    (expect equal? `(1 ,(/ x 2) ,x) '(1 20 40)))
  (let ((a 3))
    (expect equal? `((1 2) ,a ,4 ,'five 6)
                   `((1 2) 3 4 five 6)))
  (expect equal? (quasiquote (list (unquote (+ 1 2)) 4))
                 '(list 3 4))
  (expect equal? '(quasiquote (list (unquote (+ 1 2)) 4))
                 '`(list ,(+ 1 2) 4))))

(describe "quasiquote unquote nested" (lambda ()
  (expect equal? ``,1 '`,1)
  (expect equal? ``,,(+ 1 2) '`,3)

  (expect equal? `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
                 '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
  (let ((name1 'x)
        (name2 'y))
    (expect equal? `(a `(b ,,name1 ,',name2 d) e)
                   '(a `(b ,x ,'y d) e)))))

(describe "quasiquote unquote-splicing" (lambda ()
  (define (abs x)
    (if (< x 0) (- x) x))
  (define (sq x)
    (* x x))
  (let ((x '()))
    (expect equal? `(,@x) '()))
  (let ((x '(42)))
    (expect equal? `(,@x) '(42)))
  (let ((x '(1 2 3)))
    (expect equal? `(,@x) '(1 2 3)))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)))
                 '((foo 7)))
  (expect equal? `(1 ,@(cdr '(2)) 3) '(1 3))
  (expect equal? `(,@(map abs '(4 -5 6)))
                 '(4 5 6))
  (expect equal? `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
                 '(a 3 4 5 6 b))
  (expect equal? `(10 5 ,(sq 2) ,@(map sq '(4 3)) 8)
                 '(10 5 4 16 9 8))))

(describe "quasiquote improper lists" (lambda ()
  (expect equal? `(1 . 2) '(1 . 2))
  (expect equal? `(1 . ,(car '(2))) '(1 . 2))
  (expect equal? `((foo ,(- 10 3)) ,@(list 8) . ,(car '(9))) '((foo 7) 8 . 9))
  (expect equal? `((foo ,(- 10 3)) ,@(list 8) . ,(car '(cons))) '((foo 7) 8 . cons))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(9))) '((foo 7) . 9))
  (expect equal? `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) '((foo 7) . cons))))

;; 4.3. Macros
;; 4.3.2. Pattern language
;; (describe "syntax-rules" ...)

;; 5. Program structure
;; 5.2. Definitions
(describe "define variable" (lambda ()
  (let ()
    (define x 42)
    (expect = x 42))
  (let ()
    (define x (* -1 42))
     (expect = x -42))))

(describe "define function" (lambda ()
  (let ()
    (define (f) 42)
    (expect = (f) 42))
  (let ()
    (define (f x) (* -1 x))
    (expect = (f 42) -42))))

(describe "define function variadic" (lambda ()
  (let ()
    (define (f . a) 42)
    (expect = (f) 42))
  (let ()
    (define (f . a) (* -1 (car a)))
    (expect = (f 42) -42))))

(describe "define and lambda" (lambda ()
  (let ()
    (define f (lambda () (g)))
    (define g (lambda () 42))
    (expect = (f) 42))))

;; 5.3. Syntax definitions
;; (describe "define-syntax" ...)

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
  (let ()
    (define gen-counter
      (lambda ()
        (let ((n 0))
          (lambda () (set! n (+ n 1)) n))))
    (let ((g (gen-counter)))
      (expect eqv? g g))
    (noexpect eqv? (gen-counter) (gen-counter)))

  (let ()
    (define gen-loser
      (lambda ()
        (let ((n 0))
          (lambda () (set! n (+ n 1)) 27))))
    (let ((g (gen-loser)))
      (expect eqv? g g)))

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
  (noexpect eq? '() '(1))
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
  (expect equal? '() '())
  (noexpect equal? '() '(1))
  (let ((x '(1)))
    (expect equal? x x))
  (let ((p (lambda (x) x)))
    (expect equal? p p))
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

  (if local? (begin
    (noexpect zero? 'a)
    (noexpect zero? '())
    (noexpect zero? '(1))
    (noexpect zero? "1")
    (noexpect zero? #t)
    (noexpect zero? #f)
    (noexpect zero? zero?)))))

(describe "positive?" (lambda ()
  (expect positive? 1)
  (expect positive? 1000000)

  (noexpect positive? 0)
  (noexpect positive? -1)
  (noexpect positive? -99999)

  (if local? (begin
    (noexpect positive? 'a)
    (noexpect positive? '())
    (noexpect positive? '(1))
    (noexpect positive? "1")
    (noexpect positive? #t)
    (noexpect positive? #f)
    (noexpect positive? positive?)))))

(describe "negative?" (lambda ()
  (expect negative? -1)
  (expect negative? -1000000)

  (noexpect negative? 0)
  (noexpect negative? 1)
  (noexpect negative? 1000000)

  (if local? (begin
    (noexpect negative? 'a)
    (noexpect negative? '())
    (noexpect negative? '(1))
    (noexpect negative? "1")
    (noexpect negative? #t)
    (noexpect negative? #f)
    (noexpect negative? negative?)))))

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

  (if local? (begin
    (noexpect odd? 'a)
    (noexpect odd? '())
    (noexpect odd? '(1))
    (noexpect odd? "1")
    (noexpect odd? #t)
    (noexpect odd? #f)
    (noexpect odd? odd?)))))

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

  (if local? (begin
    (noexpect even? 'a)
    (noexpect even? '())
    (noexpect even? '(1))
    (noexpect even? "1")
    (noexpect even? #t)
    (noexpect even? #f)
    (noexpect even? even?)))))

(describe "max" (lambda ()
  (expect = (max 0) 0)
  (expect = (max 0 2 1) 2)
  (expect = (max 0 0 0) 0)
  (expect = (max -10) -10)
  (expect = (max -10 -20 -30) -10)
  (expect = (max -1 -1 -1) -1)))

(describe "min" (lambda ()
  (expect = (min 0) 0)
  (expect = (min 0 2 1) 0)
  (expect = (min 2 2 2) 2)
  (expect = (min -10) -10)
  (expect = (min -10 -20 -30) -30)
  (expect = (min -1 -1 -1) -1)))

(describe "+" (lambda ()
  (expect = (+ 42 21) 63)))

(describe "-" (lambda ()
  (expect = (- 42 21) 21)))

(describe "*" (lambda ()
  (expect = (* 4 2) 8)))

(describe "/" (lambda ()
  (expect = (/ 4 2) 2)))

(describe "abs" (lambda ()
  (expect = (abs 0) 0)
  (expect = (abs 1) 1)
  (expect = (abs -1) 1)
  (expect = (abs -12345678) 12345678)))

(describe "arithmetic" (lambda ()
  (expect = (+ (+ 40 2) 21) 63)
  (expect = (+ (- 40 4) (* 3 (/ 100 50))) 42)))

(describe "quotient" (lambda ()
  (expect = (quotient 1 1) 1)
  (expect = (quotient 0 1) 0)
  (expect = (quotient 0 4) 0)
  (expect = (quotient 8 4) 2)
  (expect = (quotient 12 4) 3)
  (expect = (quotient -12 4) -3)
  (expect = (quotient 12 -4) -3)
  (expect = (quotient -12 -4) 3)))

(describe "remainder" (lambda ()
  (expect = (remainder 13 4) 1)
  (expect = (remainder -13 4) -1)
  (expect = (remainder 13 -4) 1)
  (expect = (remainder -13 -4) -1)))

(describe "modulo" (lambda ()
  (expect = (modulo 13 4) 1)
  (expect = (modulo -13 4) 3)
  (expect = (modulo 13 -4) -3)
  (expect = (modulo -13 -4) -1)))

(describe "expt" (lambda ()
  (expect = (expt 1 1) 1)
  (expect = (expt 2 2) 4)
  (expect = (expt 2 3) 8)
  (expect = (expt 2 8) 256)
  (expect = (expt -2 2) 4)
  (expect = (expt 1 0) 1)
  (expect = (expt -1 0) 1)
  (expect = (expt 0 1) 0)
  (expect = (expt 0 0) 1)
  (expect = (expt 2 15) 32768)
  (expect = (expt 2 16) 65536)
  (expect = (expt 2 24) 16777216)))

;; 6.2.6. Numerical input and output
(describe "number->string" (lambda ()
  (expect equal? (number->string 0) "0")
  (expect equal? (number->string 1) "1")
  (expect equal? (number->string -1) "-1")
  (expect equal? (number->string 16777216) "16777216")
  (expect equal? (number->string -16777216) "-16777216")))

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
  (expect = (car '(1 . 2)) 1)))

(describe "cdr" (lambda ()
  (expect = (cdr '(1 . 2)) 2)))

(describe "cxxr" (lambda ()
  (define l '(1 2 3 4))
  (expect = (cadr l) 2)
  (expect = (caddr l) 3)
  (expect = (cadddr l) 4)))

(describe "set-car!" (lambda ()
  (define l (list 'not-a-constant-list))
  (define m '(constant-list))
  (set-car! l 3)
  (expect equal? l '(3))))

(describe "set-cdr!" (lambda ()
  (define l (list 'not-a-constant-list))
  (define m '(constant-list))
  (set-cdr! l '(3))
  (expect equal? l '(not-a-constant-list 3))))

(describe "null?" (lambda ()
  (expect null? '())
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
    (expect = (length l) 2)
    (expect = (car l) 42)
    (expect equal? (car (cdr l)) "foo"))))

(describe "length" (lambda ()
  (expect = (length '()) 0)
  (expect = (length '(1)) 1)
  (expect = (length '(1 2 3 4)) 4)))

(describe "append" (lambda ()
  (expect equal? '(x y) (append '(x) '(y)))
  (expect equal? '(a b c d) (append '(a) '(b c d)))
  (expect equal? (append '(a (b)) '((c))) '(a (b) (c)))
  (expect equal? (append '(a b) '(c . d)) '(a b c . d))
  (expect equal? (append '() 'a) 'a)

  (expect null? (append))
  (expect null? (append '()))
  (expect null? (append '() '()))
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
          '(1 2 3))
  (let* ((l '(a))
         (m '(b c))
         (ret (append l m)))
    (expect eqv? (cdr ret) m); shares the last list
    (noexpect eqv? l ret)))) ; newly allocated

(describe "reverse" (lambda ()
  (expect equal? '() (reverse '()))
  (expect equal? '(1) (reverse '(1)))
  (expect equal? '(2 1) (reverse '(1 2)))
  (expect equal? '(3 2 1) (reverse '(1 2 3)))))

(describe "list-tail" (lambda ()
  (expect equal? (list-tail '() 0) '())
  (expect equal? (list-tail '(a b c d) 0) '(a b c d))
  (expect equal? (list-tail '(a b c d) 2) '(c d))
  (expect equal? (list-tail '(a b c d) 3) '(d))))

(describe "list-ref" (lambda ()
  (expect eq? (list-ref '(a b c d) 0) 'a)
  (expect eq? (list-ref '(a b c d) 2) 'c)
  (expect eq? (list-ref '(a b c d) 3) 'd)))

(describe "memq" (lambda ()
  (expect-f (memq 'a '()))
  (expect equal? (memq 'a '(a b c)) '(a b c))
  (expect equal? (memq 'b '(a b c)) '(b c))
  (expect-f (memq 'a '(b c d)))))

(describe "memv" (lambda ()
  (expect-f (memv 'a '()))
  (expect equal? (memv 'a '(a b c)) '(a b c))
  (expect equal? (memv 'b '(a b c)) '(b c))
  (expect-f (memv 'a '(b c d)))
  (expect equal? (memv 101 '(100 101 102)) '(101 102))))

(describe "member" (lambda ()
  (expect-f (member 'a '()))
  (expect equal? (member 'a '(a b c)) '(a b c))
  (expect equal? (member 'b '(a b c)) '(b c))
  (expect-f (member 'a '(b c d)))
  (expect equal? (member 101 '(100 101 102)) '(101 102))
  (expect equal? (member (list 'a) '(b (a) c)) '((a) c))))

(describe "assq" (lambda ()
  (define e '((a 1) (b 2) (c 3)))
  (expect-f (assq 'a ()))
  (expect equal? (assq 'a e) '(a 1))
  (expect equal? (assq 'b e) '(b 2))
  (expect-f (assq 'd e))
  (expect-f (assq (list 'a) '(((a)) ((b)) ((c)))))))

(describe "assv" (lambda ()
  (define e '((a 1) (b 2) (c 3)))
  (expect equal? (assv 'a e) '(a 1))
  (expect equal? (assv 'b e) '(b 2))
  (expect-f (assv 'd e))
  (expect-f (assv (list 'a) '(((a)) ((b)) ((c)))))
  (expect equal? (assv 5 '((2 3) (5 7) (11 13)))
                 '(5 7))))

(describe "assoc" (lambda ()
  (define e '((a 1) (b 2) (c 3)))
  (expect-f (assoc 'a ()))
  (expect equal? (assoc 'a e) '(a 1))
  (expect equal? (assoc 'b e) '(b 2))
  (expect-f (assoc 'd e))
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
  (expect = (string-length "foo") 3)
  (expect = (string-length "o") 1)
  (expect = (string-length "") 0)))

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

(describe "substring" (lambda ()
  (expect equal? (substring "abc" 0 0) "")
  (expect equal? (substring "abc" 0 1) "a")
  (expect equal? (substring "abc" 0 2) "ab")
  (expect equal? (substring "abc" 0 3) "abc")
  (expect equal? (substring "abc" 1 1) "")
  (expect equal? (substring "abc" 1 2) "b")
  (expect equal? (substring "abc" 1 3) "bc")
  (expect equal? (substring "abc" 2 2) "")
  (expect equal? (substring "abc" 2 3) "c")
  (expect equal? (substring "abc" 3 3) "")))

(describe "string-append" (lambda ()
  (expect equal? (string-append) "")
  (expect equal? (string-append "") "")
  (expect equal? (string-append "a") "a")
  (expect equal? (string-append "a" "bc") "abc")
  (expect equal? (string-append "(" "foo" ")") "(foo)")
  (expect equal? (string-append "(" "f" "" "o" "o" "" ")") "(foo)")))

;; 6.3.6. Vectors
(describe "vector" (lambda ()
  (expect equal? #() (vector))
  (expect equal? #(1 2 3) (vector 1 2 3))
  (expect equal? #(a 42 "foo") (vector 'a 42 "foo"))))

(describe "vector?" (lambda ()
  (expect vector? (vector))
  (expect vector? (vector 1 'a 2))))

(describe "make-vector" (lambda ()
  (define v0 (make-vector 0))
  (define v (make-vector 3 0))

  (expect vector? v0)
  (expect = (vector-length v0) 0)

  (expect vector? v)
  (expect = (vector-length v) 3)
  (expect = (vector-ref v 0) 0)
  (expect = (vector-ref v 1) 0)
  (expect = (vector-ref v 2) 0)))

(describe "vector-length" (lambda ()
  (expect = (vector-length (vector)) 0)
  (expect = (vector-length (vector 1 2 3)) 3)))

(describe "vector-ref" (lambda ()
  (let ((v (vector 0 1 2)))
    (expect eq? (vector-ref v 0) 0)
    (expect eq? (vector-ref v 2) 2)
    (if local?
        (expect-f (vector-ref v 9))))))

(describe "vector-set!" (lambda ()
  (let ((v (vector 0 1 2)))
    (expect = (vector-ref v 1) 1)
    (vector-set! v 1 42)
    (expect = (vector-ref v 1) 42))))

;; 6.4. Control features
(describe "procedure?" (lambda ()
  (expect procedure? car)
  (noexpect procedure? 'car)
  (expect procedure? (lambda (x) (* x x)))
  (noexpect procedure? '(lambda (x) (* x x)))
  (expect-t (call/cc (lambda (c) (procedure? c))))))

(describe "apply" (lambda ()
  (expect = (apply + '()) 0)
  (expect = (apply + '(42)) 42)
  (expect = (apply + 1 '(42)) 43)
  (expect = (apply + 1 2 '(42)) 45)
  (expect = (apply + 1 2 3 '(42)) 48)
  (expect = (apply + '(1 2 3 42)) 48)
  (expect = (apply + (apply + '(1 2 3)) '(42)) 48)
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
    (expect = x 25))
  (let ((x 0))
    (for-each (lambda (a b) (set! x (+ a b))) '(1 2 3) '(4 5 6))
    (expect = x 9))
  (let ((x 0))
    (for-each (lambda (a b) (set! x (+ a b))) '(1 2) '(4))
    (expect = x 5))))

(describe "call-with-current-continuation" (lambda ()
  (define (f)
    (call-with-current-continuation
     (lambda (exit)
       (for-each (lambda (x)
                   (if (negative? x)
                       (exit x)))
                 '(54 0 37 -3 245 19))
       #t)))
  (define list-length
    (lambda (obj)
      (call-with-current-continuation
       (lambda (return)
         (letrec ((r
                   (lambda (obj)
                     (cond ((null? obj) 0)
                           ((pair? obj)
                            (+ (r (cdr obj)) 1))
                           (else (return #f))))))
           (r obj))))))

  (expect = (f) -3)
  (expect = (list-length '(1 2 3 4)) 4)
  (expect-f (list-length '(a b . c)))))

(describe "values and call-with-values" (lambda ()
  (expect =
          (call-with-values (lambda () (values 4 5))
            (lambda (a b) b))
          5)
  (expect = (call-with-values * -) -1)))

(describe "values and call-with-values nested" (lambda ()
  (expect = (call-with-values
              (lambda ()
                (call-with-values
                  (lambda () (values 4 5))
                  (lambda (a b) b))); returns 5
              (lambda (x) (* 2 x)))
          10)))

(describe "call/cc applicable in call/cc" (lambda ()
  (define (f)
    (call/cc call/cc)
    42)
  (expect = (f) 42)))

;; 6.5. Eval
(describe "eval" (lambda ()
  (expect = (eval '(* 7 3) (scheme-report-environment 5)) 21)
  (let ((f (eval '(lambda (f x) (f x x))
                 (null-environment 5))))
    (expect = (f + 10) 20))))

(describe "*-environment" (lambda ()
  (define r5rs-env (scheme-report-environment 5))
  (define null-env (null-environment 5))
  (define int-env (interaction-environment))
  (expect-f (eq? r5rs-env #f))
  (expect-f (eq? null-env #f))
  (expect-f (eq? int-env #f))
  (noexpect equal? r5rs-env null-env)
  (noexpect equal? r5rs-env int-env)))

;; 6.6. Input and output
;; 6.6.1. Ports
(describe "port?" (lambda ()
  (noexpect port? 'port)
  (noexpect port? #f)))

(describe "current-input-port" (lambda ()
  (expect port? (current-input-port)) ; test for twice
  (expect port? (current-input-port))))

(describe "current-output-port" (lambda ()
  (expect port? (current-output-port)) ; test for twice
  (expect port? (current-output-port))))

(describe "input-port?" (lambda ()
  (expect input-port? (current-input-port))
  (noexpect input-port? (current-output-port))))

(describe "output-port?" (lambda ()
  (noexpect output-port? (current-input-port))
  (expect output-port? (current-output-port))))

(describe "open-input-file" (lambda ()
  (let ((p (open-input-file "/dev/null")))
    (expect input-port? p)
    (close-input-port p))))

(describe "open-output-file" (lambda ()
  (let ((p (open-output-file "/dev/null")))
    (expect output-port? p)
    (close-input-port p))))

(describe "close-input-port" (lambda ()
  (let ((p (open-input-file "/dev/null")))
    (close-input-port p)
    (expect-t #t))))

(describe "close-output-port" (lambda ()
  (let ((p (open-output-file "/dev/null")))
    (close-output-port p)
    (expect-t #t))))

(describe "call-with-input-file" (lambda ()
  (define pp #f)
  (call-with-input-file "/dev/null"
    (lambda (p)
      (expect input-port? p)
      (set! pp p)))
  (expect input-port? pp)));; closed

(describe "call-with-output-file" (lambda ()
  (define pp #f)
  (call-with-output-file "/dev/null"
    (lambda (p)
      (expect output-port? p)
      (set! pp p)))
  (expect output-port? pp)));; closed

(describe "with-input-from-file" (lambda ()
  (define pp #f)
  (define stdin (current-input-port))
  (with-input-from-file "/dev/null"
    (lambda ()
      (define in (current-input-port))
      (expect input-port? in)
      (noexpect equal? in stdin)
      (set! pp in)))
  (expect input-port? pp);; closed
  (noexpect equal? pp (current-input-port))
  (expect equal? stdin (current-input-port))))

(describe "with-output-to-file" (lambda ()
  (define pp #f)
  (define stdout (current-output-port))
  (with-output-to-file "/dev/null"
    (lambda ()
      (define out (current-output-port))
      (expect output-port? out)
      (noexpect equal? out stdout)
      (set! pp out)))
  (expect output-port? pp);; closed
  (noexpect equal? pp (current-output-port))
  (expect equal? stdout (current-output-port))))

;; 6.6.2. Input
(define (read-file f)
  (with-input-from-file f read))

(describe "read" (lambda ()
  (define fact '(define (fact n)
                  (if (<= n 2)
                      n
                      (* n (fact (- n 1))))))
  (expect equal? (read-file "test/data-trivial.txt") '(1 2 "abc"))
  (expect equal? (read-file "test/data-fact.txt") fact)))

(describe "read /dev/null" (lambda ()
  (expect eof-object? (read-file "/dev/null"))))

(describe "read twice" (lambda ()
  (with-input-from-file "test/data-double.txt"
    (lambda ()
      (expect equal? (read) '(1 2 "abc"))
      (expect equal? (read) '(3 4 "def"))))))

;; 6.6.3. Output

(describe "display" (lambda ()
  (call-with-output-file "/dev/null"
    (lambda (p)
     (display "\"testing\"" p)))))

(define (expect-no-stuck-on-display obj)
  (with-output-to-file "/dev/null"
    (lambda ()
      (display obj)
      (expect-t #t))))

(describe "display no stuck on circular list car" (lambda ()
  (define l (list 42))
  (set-car! l l);; make it circular!
  (expect-no-stuck-on-display l)))

(describe "display no stuck on circular list cdr" (lambda ()
  (define l (list 42))
  (set-cdr! l l);; !
  (expect-no-stuck-on-display l)))

(describe "display no stuck on circular list car but cdr" (lambda ()
  (define l (list 1 2))
  (set-car! l l);; !
  (expect-no-stuck-on-display l)))

(describe "display no stuck on circular list car and cdr" (lambda ()
  (define l (list 1 2))
  (set-car! l l);; !
  (set-cdr! l l);; !
  (expect-no-stuck-on-display l)))

(describe "display no stuck on circular vector" (lambda ()
  (define v #(#f))
  (vector-set! v 0 v);; !
  (expect-no-stuck-on-display v)))

(describe "display no stuck on circular list AND vector" (lambda ()
  (define l (list 0))
  (define v (vector l))
  (set-car! l v);; !
  (expect-no-stuck-on-display l)
  (expect-no-stuck-on-display v)))

(describe "newline" (lambda ()
  (call-with-output-file "/dev/null"
    (lambda (p)
      (display "\"a" p)
      (newline p)
      (display "b\"" p)))))

;; 6.6.4. System interface
(describe "load" (lambda ()
  (load "./data-fact.txt")
  (expect = (fact 5) 120)))

(load "./test-callcc.scm")

;;
;; R7RS
;;

(define (call-with-output-string proc)
  (let* ((p (open-output-string))
         (ret (proc p)))
    (close-output-port p)
    ret))

(if r7rs? (begin
  (describe "close-port" (lambda ()
    (let ((p (open-input-file "/dev/null")))
      (close-port p)
      (expect-t #t))))

  (define (read-string-from-data-trivial k)
    (with-input-from-file "test/data-trivial.txt"
      (lambda () (read-string k))))

  (describe "read-string" (lambda ()
    (expect equal? (read-string-from-data-trivial 0) "")
    (expect equal? (read-string-from-data-trivial 1) "(")
    (expect equal? (read-string-from-data-trivial 4) "(1 2")
    (expect equal? (read-string-from-data-trivial 12) "(1 2 \"abc\")\n")
    (expect equal? (read-string-from-data-trivial 999) "(1 2 \"abc\")\n")))

  (describe "read-string EOF" (lambda ()
    (with-input-from-file "test/data-trivial.txt"
      (lambda ()
        (read-string 999)
        (expect eof-object? (read-string 1))))))

  (describe "open/get-output-string" (lambda ()
    (call-with-output-string
     (lambda (p)
       (expect port? p)
       (expect string=? (get-output-string p) "")
       (display "hello!" p)
       (expect string=? (get-output-string p) "hello!")))))))

;;
;; Local Extensions
;;

(if local? (begin
  (describe "_cputime" (lambda ()
    (let ((t (_cputime)))
      (expect number? t)
      (expect > t 0))))

  (describe "_defined?" (lambda ()
    (define a 10)
    (define (f) (_defined? a))
    (define (f2) (_defined? b))
    (let ((g (lambda () (_defined? a)))
          (g2 (lambda () (_defined? b))))
      (expect-t (_defined? a))
      (expect-f (_defined? b))
      (expect-t (f))
      (expect-f (f2))
      (expect-t (g))
      (expect-f (g2)))))

  (describe "schaf-environment" (lambda ()
    (define schaf-env (schaf-environment))
    (define r5rs-env (scheme-report-environment 5))
    (expect-f (eq? schaf-env #f))
    (noexpect equal? schaf-env r5rs-env)))

  (describe "eval with schaf-environment" (lambda ()
    (expect = (eval '(* 7 3) (schaf-environment)) 21)
    (let ((f (eval '(lambda (f x) (f x x))
                   (schaf-environment))))
      (expect = (f + 10) 20))))

  ;; Detailed and implementation-dependent specs

  (define (stringify obj)
    (call-with-output-string
     (lambda (p)
       (display obj p)
       (get-output-string p))))

  (describe "display circular list car" (lambda ()
    (define l (list 42))
    (set-car! l l);; make it circular!
    (expect equal? (stringify l) "(..)")))

  (describe "display circular list car and cdr" (lambda ()
    (define l (list 1 2))
    (set-car! l l);; !
    (set-cdr! l l);; !
    (expect equal? (stringify l) "(.. ..)")))

  (describe "display circular list car but cdr" (lambda ()
    (define l (list 1 2))
    (set-car! l l);; !
    (expect equal? (stringify l) "(.. 2)")))

  (describe "display circular list cdr" (lambda ()
    (define l (list 42))
    (set-cdr! l l);; !
    (expect equal? (stringify l) "(42 ..)")))

  (describe "display circular list cdr at 3rd" (lambda ()
    (define l (list 1 2 #f))
    (set-cdr! (cdr l) l);; !
    (expect equal? (stringify l) "(1 2 ..)")))

  (describe "display circular list mid" (lambda ()
    (define l (list 1 #f 2))
    (set-car! (cdr l) l);; !
    (expect equal? (stringify l) "(1 .. 2)")))

  (describe "display circular list car of 3" (lambda ()
    (define l (list 0 1 2))
    (set-car! l l);; !
    (expect equal? (stringify l) "(.. 1 2)")))

  (describe "display circular list mid twice" (lambda ()
   (define l (list 1 #f #f 2))
   (set-car! (cdr l) l);; !
   (set-car! (cddr l) l);; !
   (expect equal? (stringify l) "(1 .. .. 2)")))

  (describe "display nested list" (lambda ()
   (expect equal? (stringify '((0))) "((0))")))

  (describe "display nested vector" (lambda ()
   (expect equal? (stringify #(#(0))) "#(#(0))")))

  (describe "display mutually nested list and vector" (lambda ()
   (expect equal? (stringify #((0))) "#((0))")
   (expect equal? (stringify '(#(0))) "(#(0))")))))

(test-run)
