(import (rnrs) (algos))

;; Simple assert helper
(define (assert condition label)
  (if condition
      (begin (display "PASS: ") (display label) (newline))
      (begin (display "FAIL: ") (display label) (newline))))

;; Helper: build a graph
(define pA (make-point 0 0))
(define pB (make-point 1 0))
(define pC (make-point 2 0))
(define pD (make-point 3 0))
(define pE (make-point 4 0))
(define pF (make-point 5 0))

;; Construct nodes (note: edges must refer to actual nodes, not points)
(define node-f (make-node pF '()))
(define node-e (make-node pE (list node-f)))
(define node-d (make-node pD '()))
(define node-c (make-node pC (list node-d node-e)))
(define node-b (make-node pB (list node-c)))
(define node-a (make-node pA (list node-b)))

;; Simple reachable test
(define path1 (dfs node-a node-f))
(assert path1 "DFS should find a path from A to F")

;; Unreachable test
(define isolated (make-node (make-point 99 99) '()))
(define path2 (dfs node-a isolated))
(assert (not path2) "DFS should not find a path to isolated node")

;; Path correctness (not required, but sanity check)
(define (point=? p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(define (starts-at path pt)
  (point=? (node-value (car path)) pt))

(define (ends-at path pt)
  (point=? (node-value (car (reverse path))) pt))

(assert (and path1 (starts-at path1 pA) (ends-at path1 pF))
        "DFS path from A to F starts at A and ends at F")

(display "All DFS tests completed.\n")

