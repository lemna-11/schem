#!r6rs
    
;;; shared structure utilities

(library (srfi :166 show-shared)
  (export
   extract-shared-objects call-with-shared-ref call-with-shared-ref/cdr)
  (import (rnrs)
          (rnrs mutable-pairs)
          (only (srfi :69)
                make-hash-table
                hash-table-delete!
                hash-table-ref
                hash-table-ref/default
                hash-table-set!
                hash-table-update!/default
                hash-table-walk))
  (begin
    (define (extract-shared-objects x cyclic-only?)
      (let ((seen (make-hash-table eq?)))
        ;; find shared references
        (let find ((x x))
          (cond ;; only interested in pairs and vectors (and records later)
           ((or (pair? x) (vector? x))
            ;; increment the count
            (hash-table-update!/default seen x (lambda (n) (+ n 1)) 0)
            ;; walk if this is the first time
            (cond
             ((> (hash-table-ref seen x) 1))
             ((pair? x)
              (find (car x))
              (find (cdr x)))
             ((vector? x)
              (do ((i 0 (+ i 1)))
                  ((= i (vector-length x)))
                (find (vector-ref x i)))))
            ;; delete if this shouldn't count as a shared reference
            (if (and cyclic-only? (<= (hash-table-ref/default seen x 0) 1))
                (hash-table-delete! seen x)))))
        ;; extract shared references
        (let ((res (make-hash-table eq?))
              (count 0))
          (hash-table-walk
           seen
           (lambda (k v)
             (cond
              ((> v 1)
               (hash-table-set! res k (cons count #f))
               (set! count (+ count 1))))))
          (cons res 0))))

    (define (gen-shared-ref cell shares)
      (set-car! cell (cdr shares))
      (set-cdr! cell #t)
      (set-cdr! shares (+ (cdr shares) 1))
      (string-append (number->string (car cell))))

    (define (call-with-shared-ref obj shares each proc)
      (let ((cell (hash-table-ref/default (car shares) obj #f)))
        (cond
         ((and (pair? cell) (cdr cell))
          (each "#" (number->string (car cell)) "#"))
         ((pair? cell)
          (each "#" (gen-shared-ref cell shares) "=" proc))
         (else
          (each proc)))))

    (define (call-with-shared-ref/cdr obj shares each proc . o)
      (let ((sep (if (pair? o) (car o) ""))
            (cell (hash-table-ref/default (car shares) obj #f)))
        (cond
         ((and (pair? cell) (cdr cell))
          (each sep ". #" (number->string (car cell)) "#"))
         ((pair? cell)
          (each sep ". #" (gen-shared-ref cell shares) "=(" proc ")"))
         (else
          (each sep proc)))))
    ))
