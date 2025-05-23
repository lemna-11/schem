(import (rnrs) (dynarray))

(define (assert msg condition)
  (if condition
      (display (string-append "PASS: " msg "\n"))
      (error (string-append "FAIL: " msg))))

;; Test make-dynarray and initial length
(let ((arr (make-dynarray)))
  (assert "Initial length is 0" (= (dynarray-length arr) 0)))

;; Test push! and get
(let ((arr (make-dynarray)))
  (dynarray-push! arr 10)
  (dynarray-push! arr 20)
  (assert "Length after 2 pushes is 2" (= (dynarray-length arr) 2))
  (assert "First element is 10" (= (dynarray-get arr 0) 10))
  (assert "Second element is 20" (= (dynarray-get arr 1) 20)))

;; Test set!
(let ((arr (make-dynarray)))
  (dynarray-push! arr 1)
  (dynarray-push! arr 2)
  (dynarray-set! arr 1 42)
  (assert "After set!, second element is 42" (= (dynarray-get arr 1) 42)))

;; Test delete!
(let ((arr (make-dynarray)))
  (dynarray-push! arr 5)
  (dynarray-push! arr 6)
  (dynarray-push! arr 7)
  (dynarray-delete! arr 1)  ;; remove the 6
  (assert "Length after delete is 2" (= (dynarray-length arr) 2))
  (assert "Element at index 1 after delete is 7" (= (dynarray-get arr 1) 7)))

;; Test swap!
(let ((arr (make-dynarray)))
  (dynarray-push! arr 'a)
  (dynarray-push! arr 'b)
  (dynarray-push! arr 'c)
  (dynarray-swap! arr 0 2)
  (assert "After swap!, index 0 is 'c'" (eq? (dynarray-get arr 0) 'c))
  (assert "After swap!, index 2 is 'a'" (eq? (dynarray-get arr 2) 'a)))

;; Test capacity doubling (internal)
(let ((arr (make-dynarray)))
  (let loop ((i 0))
    (when (< i 10)
      (dynarray-push! arr i)
      (loop (+ i 1))))
  (assert "Length after pushing 10 elements is 10" (= (dynarray-length arr) 10))
  (assert "Element 9 is correct" (= (dynarray-get arr 9) 9)))

;; *** Test emptying the array completely ***
(let ((arr (make-dynarray)))
  (for-each (lambda (x) (dynarray-push! arr x)) '(1 2 3 4 5))
  (let loop ((i (- (dynarray-length arr) 1)))
    (when (>= i 0)
      (dynarray-delete! arr i)
      (loop (- i 1))))
  (assert "Length after emptying is 0" (= (dynarray-length arr) 0))
  (let ((error-thrown #f))
    (with-handlers ((exn:fail? (lambda (e) (set! error-thrown #t))))
      (dynarray-get arr 0))
    (assert "Accessing element from empty array throws error" error-thrown)))

;; *** Test refilling after emptying ***
(let ((arr (make-dynarray)))
  (for-each (lambda (x) (dynarray-push! arr x)) '(10 20 30))
  (assert "Length after refilling is 3" (= (dynarray-length arr) 3))
  (assert "First element after refilling is 10" (= (dynarray-get arr 0) 10))
  (assert "Second element after refilling is 20" (= (dynarray-get arr 1) 20))
  (assert "Third element after refilling is 30" (= (dynarray-get arr 2) 30)))

(display "All dynarray tests done.\n")

