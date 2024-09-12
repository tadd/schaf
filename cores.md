# Core Parts

Non-libraries

## Filtered

We'll define these as the "minimal" feature set.

* quote / `'`
* `#t`/`#f`
* lambda
* if
* set!
* define

## Syntax

* quote / `'`
* `#t`/`#f`
* lambda
* if
* set!
* quasiquote / <code>`</code>
* let-syntax
* letrec-syntax
* define (?)
* define-syntax (?)

## Procedures

* eqv?
* eq?
* number?
* complex?
* real?
* rational?
* integer?
* exact?
* inexact?
* =
* <
* >
* <=
* >=
* +
* *
* -
* /
* quotient
* remainder
* modulo
* floor
* ceiling
* truncate
* round
* exp
* log
* sin
* cos
* tan
* asin
* acos
* atan (arg 1/2)
* sqrt
* expt
* make-rectangular
* make-polar
* real-part
* imag-part
* magnitude
* angle
* exact->inexact
* inexact->exact
* number->string (arg 1/2)
* string->number (arg 1/2)
* pair?
* cons
* car
* cdr
* set-car!
* set-cdr!
* symbol?
* symbol->string
* string->symbol
* char?
* char=?
* char<?
* char>?
* char<=?
* char>=?
* char->integer
* integer->char
* string?
* make-string (arg 1/2)
* string-length
* string-ref
* string-set!
* vector?
* make-vector (arg 1/2)
* vector-length
* vector-ref
* vector-set!
* procedure?
* apply
* call-with-current-continuation
* values
* call-with-values
* dynamic-wind
* eval
* scheme-report-environment
* null-environment
* input-port?
* output-port?
* current-input-port
* current-output-port
* open-input-file
* open-output-file
* close-input-port
* close-output-port
* read-char (arg 0/1)
* peek-char (arg 0/1)
* eof-object?
* char-ready? (arg 0/1)
* write-char (arg 1/2)
