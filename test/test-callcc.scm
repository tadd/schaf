;; All of call/cc tests in this file were from Kawa's test suite under the MIT license

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
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
  (expect = (f1) 28)
  (expect = (f2) 28)))

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

(describe "call/cc + letrec = set!" (lambda ()
  (define (f)
    (letrec ((x (call/cc
		 (lambda (c)
		   (list #t c)))))
      (if (car x)
	  ((cadr x) (list #f (lambda () x)))
	  (eq? x ((cadr x))))))
  (expect-t (f))))

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

  (expect = (fa 3) 3)
  (expect = (fb 3) 13)
  (expect = (fc 3) 23)))

;; Failing tests by (maybe) bugs

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
(xdescribe "call/cc letrec" (lambda ()
  (define (f)
    (letrec ((x (call/cc list))
             (y (call/cc list)))
      (cond ((procedure? x) (x (pair? y)))
	    ((procedure? y) (y (pair? x))))
      (let ((x (car x))
            (y (car y)))
        (and (call/cc x) (call/cc y) (call/cc x)))))
  (expect-t (f))))

;; inf-loop
(xdescribe "call/cc in-yo" (lambda ()
  (define r
    (let ((x '())
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

(xdescribe "call/cc defines in letrec body" (lambda ()
  (define (f)
    (let ((cont #f))
      (letrec ((x (call/cc (lambda (c) (set! cont c) 0)))
               (y (call/cc (lambda (c) (set! cont c) 0))))
        (if cont
            (let ((c cont))
              (set! cont #f)
              (set! x 1)
              (set! y 1)
              (c 0))
            (+ x y)))))
  (expect eq? (f) 0)))
