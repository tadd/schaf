;; Failing tests by (maybe) bugs
;; All of above call/cc tests were from Kawa's test suite under the MIT license

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm

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

;; inf-loop
(describe "call/cc in-yo" (lambda ()
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

(describe "call/cc defines in retlec body" (lambda ()
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
