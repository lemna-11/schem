(library (dynarray)
  (export make-dynarray dynarray-length dynarray-get dynarray-set!
          dynarray-push! dynarray-delete! dynarray-swap!)
  (import (rnrs))

  (define-record-type (dynarray mk-array is-array?)
    (fields
      (mutable vec get-vec set-vec!)
      (mutable size get-size set-size!)))

  (define (make-dynarray)
    (mk-array (make-vector 4) 0))

  (define (dynarray-length darr)
    (get-size darr))

  (define (ensure-capacity darr)
    (let* ((vec (get-vec darr))
           (size (get-size darr))
           (capacity (vector-length vec)))
      (when (>= size capacity)
        (let* ((new-cap (* 2 capacity))
               (new-vec (make-vector new-cap)))
          (do ((i 0 (+ i 1)))
              ((= i size))
            (vector-set! new-vec i (vector-ref vec i)))
          (set-vec! darr new-vec)))))

  (define (dynarray-push! darr value)
    (ensure-capacity darr)
    (let ((size (get-size darr)))
      (vector-set! (get-vec darr) size value)
      (set-size! darr (+ size 1))))

  (define (dynarray-get darr index)
    (if (and (>= index 0) (< index (get-size darr)))
        (vector-ref (get-vec darr) index)
        (error 'dynarray-get "Index out of bounds" index)))

  (define (dynarray-set! darr index value)
    (if (and (>= index 0) (< index (get-size darr)))
        (vector-set! (get-vec darr) index value)
        (error 'dynarray-set "Index out of bounds" index)))

  (define (dynarray-delete! darr index)
    (let ((size (get-size darr)))
      (if (or (< index 0) (>= index size))
          (error 'dynarray-delete "Index out of bounds" index)
          (begin
            (do ((i index (+ i 1)))
                ((= i (- size 1)))
              (vector-set! (get-vec darr) i
                           (vector-ref (get-vec darr) (+ i 1))))
            (set-size! darr (- size 1))))))

  (define (dynarray-swap! darr i j)
    (let ((size (get-size darr)))
      (if (or (< i 0) (>= i size) (< j 0) (>= j size))
          (error 'dynarray-swap "Index out of bounds" i j)
          (let* ((vec (get-vec darr))
                 (temp (vector-ref vec i)))
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j temp)))))
)
