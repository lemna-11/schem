(import (rnrs)
        (graph-parser)
        (algos))

(define test-file "test_graph.graph")

(define test-graph-string
  "-- points
A: 0 0
B: 1 1
C: 2 2

-- nodes
n1: A [n2, n3]
n2: B []
n3: C [n2]
")

;; Write test graph to file
(call-with-output-file test-file
  (lambda (port)
    (write-string test-graph-string port)))

;; Test parsing success
(define (test1)
  (let ((graph (parse-graph-from-file test-file)))
    (assert (not (null? graph)) "Parse valid graph file - graph should not be empty")
    (assert (= (length graph) 3) "Parse valid graph file - graph should have 3 nodes")
    ;; Check node points
    (let ((n1 (car graph)))
      (assert (= (point-x (node-value n1)) 0) "Parse valid graph file - n1 point x should be 0")
      (assert (= (point-y (node-value n1)) 0) "Parse valid graph file - n1 point y should be 0"))))

(test1)

;; Test error on multiple colons
(define (test2)
  (call-with-output-file "bad.graph"
    (lambda (port)
      (write-string "A: 0: 0\n")))
  (begin
    (let/ec return
      (guard (ex (else (return #t)))
        (parse-graph-from-file "bad.graph")
        (assert #f "Parse error on multiple colons should raise error")))
    #t))

(test2)

;; Test serialization roundtrip
(define (test3)
  (let* ((graph (parse-graph-from-file test-file))
         (serialized (serialize-grap graph))
         (tmp-file "roundtrip.graph/schemamel"))

    (call-with-output-file tmp-file
      (lambda (port)
        (write-string serialized port)))

    (let ((roundtrip-graph (parse-graph-from-file tmp-file)))
      (assert (= (length graph) (length roundtrip-graph)) "Roundtrip node count should be equal")
      ;; check first node points equal (basic)
      (let ((roundtrip-n1 (car roundtrip-graph))
            (n1 (car graph)))
        (assert (= (point-x (node-value roundtrip-n1)) (point-x (node-value n1)))
        "Roundtrip point-x should be equal")))))

(test3)

(display "All graph tests completed.\n")

