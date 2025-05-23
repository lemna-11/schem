(library (test_utils)
  (export assert assert-equal)
  (import (rnrs))

  (define (assert condition label)
    (if condition
        (begin (display "PASS: ") (display label) (newline))
        (begin (display "FAIL: ") (display label) (newline))))
    
  (define (assert-equal label actual expected)
    (if (equal? actual expected)
        (begin (display "PASS: ") (display label) (newline))
        (begin 
          (display "FAIL: ") (display label) 
          (display " — expected: ") (write expected)
          (display ", got: ") (write actual)
          (newline)))))