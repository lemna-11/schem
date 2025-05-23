(import (rnrs)
        (pheap)
        (test-utils))

(define (run-pheap-tests)
  (let ()
    ;; Test helpers
    (define (assert-equal label actual expected)
      (unless (equal? actual expected)
        (display "FAIL: ") (display label)
        (display " — expected: ") (write expected)
        (display ", got: ") (write actual)
        (newline)))

    ;; Test heaps
    (define min-heap (make-pheap <))
    (define max-heap (make-pheap >))

    ;; Min-heap test
    (pheap-insert! min-heap 5)
    (pheap-insert! min-heap 3)
    (pheap-insert! min-heap 9)
    (pheap-insert! min-heap 1)

    (assert-equal "min-heap peek" (pheap-peek min-heap) 1)

    (assert-equal "min-heap pop 1" (pheap-pop! min-heap) 1)
    (assert-equal "min-heap pop 2" (pheap-pop! min-heap) 3)
    (assert-equal "min-heap pop 3" (pheap-pop! min-heap) 5)
    (assert-equal "min-heap pop 4" (pheap-pop! min-heap) 9)

    ;; Max-heap test
    (for-each (lambda (n) (pheap-insert! max-heap n)) '(2 10 4 7))

    (assert-equal "max-heap peek" (pheap-peek max-heap) 10)
    (assert-equal "max-heap pop 1" (pheap-pop! max-heap) 10)
    (assert-equal "max-heap pop 2" (pheap-pop! max-heap) 7)
    (assert-equal "max-heap pop 3" (pheap-pop! max-heap) 4)
    (assert-equal "max-heap pop 4" (pheap-pop! max-heap) 2)

    (display "All tests completed.") (newline)))

(run-pheap-tests)

