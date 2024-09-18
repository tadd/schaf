;; Failing tests by (maybe) bugs
;; All of above call/cc tests were from Kawa's test suite under the MIT license

;; https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
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

(describe "call/cc retlec and set!" (lambda ()
  (define (f)
    (letrec ((x (call/cc
		 (lambda (c)
		   (list #t c)))))
      (if (car x)
	  ((cadr x) (list #f (lambda () x)))
	  (eq? x ((cadr x))))))
  (expect-t (f))))
