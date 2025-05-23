(library (pheap)
  (export make-pheap pheap-pop! pheap-peek pheap-insert! pheap-empty?)
  (import (rnrs) (dynarray))
  
  (define-record-type (pheap mk-pheap is-pheap?)
    (fields
      (immutable dynarr)
      (immutable comparator)))

  (define (make-pheap comparator)
    (mk-pheap (make-dynarray) comparator))

  (define (pop-heapify! heap i)
    (let* ((array (pheap-dynarr heap))
           (comp (pheap-comparator heap))
           (len (dynarray-length array))
           (left-idx (+ (* i 2) 1))
           (right-idx (+ (* i 2) 2))
           (smallest i))

      (when (< left-idx len)
        (let ((left-val (dynarray-get array left-idx))
              (curr-val (dynarray-get array smallest)))
          (when (not (comp curr-val left-val))
            (set! smallest left-idx))))

      (when (< right-idx len)
        (let ((right-val (dynarray-get array right-idx))
              (smallest-val (dynarray-get array smallest)))
          (when (not (comp smallest-val right-val))
            (set! smallest right-idx))))

      (when (not (= smallest i))
        (dynarray-swap! array i smallest)
        (pop-heapify! heap smallest))))

  (define (pheap-pop! heap)
    (let* ((array (pheap-dynarr heap))
           (head (dynarray-get array 0))
           (back-idx (- (dynarray-length array) 1)))
      (dynarray-swap! array 0 back-idx)
      (dynarray-delete! array back-idx)
      (pop-heapify! heap 0)
      head))

  (define (pheap-peek heap)
    (dynarray-get (pheap-dynarr heap) 0))

  (define (insert-heapify! heap i)
    (let* ((parent (floor (/ (- i 1) 2)))
           (array (pheap-dynarr heap)))

      (when (>= parent 0)
        (let* ((heapi (dynarray-get array i))
               (heappa (dynarray-get array parent))
               (comp (pheap-comparator heap)))

          (when (comp heapi heappa)
            (dynarray-swap! array i parent)
            (insert-heapify! heap parent))))))

  (define (pheap-insert! heap value)
   (let* ((array (pheap-dynarr heap)))

    (dynarray-push! array value)
    (insert-heapify! heap (- (dynarray-length array) 1))))

  (define (pheap-empty? heap)
    (let ((arr (pheap-dynarr heap)))
      (= 0 (dynarray-length arr))))
)
