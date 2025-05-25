(library (algos)
  (export dfs make-point make-node node-value point-x point-y node-edges node-name point-name)
  (import (rnrs) (pheap))

  (define-record-type (point make-point is-point?)
    (fields (immutable name)
            (immutable x)
            (immutable y)))

  (define (distance p1 p2)
    (let ((dx (- (point-x p1) (point-x p2)))
          (dy (- (point-y p1) (point-y p2))))
      (sqrt (+ (* dx dx) (* dy dy)))))

  (define (p=? p1 p2)
    (and (= (point-x p1) (point-x p2))
         (= (point-y p1) (point-y p2))))

  (define-record-type (node make-node is-node?)
    (fields 
      (immutable name)
      (immutable value)
      (immutable edges)))

  (define (dfs start end)
    (let ((heap (make-pheap (lambda (n1 n2)
                              (< (distance (node-value n1) (node-value end))
                                 (distance (node-value n2) (node-value end))))))
          (visited '()))

      (define (point-member? pt lst)
        (cond
          ((null? lst) #f)
          ((p=? pt (car lst)) #t)
          (else (point-member? pt (cdr lst)))))

      (define (dfs-rec queue path)
        (if (pheap-empty? queue)
            #f
            (let ((cur (pheap-pop! queue)))
              (if (point-member? (node-value cur) visited)
                  (dfs-rec queue path)
                  (begin
                    (set! visited (cons (node-value cur) visited))
                    (if (p=? (node-value cur) (node-value end))
                        (reverse (cons cur path))
                        (begin
                          (for-each (lambda (n) (pheap-insert! queue n))
                                    (node-edges cur))
                          (dfs-rec queue (cons cur path)))))))))

      (pheap-insert! heap start)
      (dfs-rec heap '())))

  (define dijkstra-algo '())
)
