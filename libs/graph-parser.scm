(library (graph-parser)
  (export 
    parse-graph-from-file
    serialize-graph)
  (import (algos) (chezscheme) (srfi :13))

  ; Helper function to check if a string starts with another string.
  (define (string-starts-with? str prefix)
    (let ((str-len (string-length str))
          (prefix-len (string-length prefix)))
      (and (>= str-len prefix-len) (string=? (substring str 0 prefix-len) prefix))))
  
  ;; Tokenizer: returns either ('section .points) or ('section .nodes) or a list starting with a symbol and tokens
  (define (tokenize line line-num)
    (cond
      ((string-starts-with? line "--")
       (list 'section (string-trim (substring line 2))))
      (else
       (let ((colon-count (string-count line #\:)))
         (if (not (= colon-count 1))
             (error 'tokenize (format "Line ~a: expected exactly one colon, found ~a" line-num colon-count))
             (let* ((parts (string-split line #\:))
                    (left (string-trim (car parts)))
                    (right (string-trim (cadr parts))))
               (cons (string->symbol left)
                     (string-tokenize right))))))))

  ;; Simple tokenizer by whitespace and punctuation
  (define (string-tokenize str)
    (define (is-separator? c)
      (or (char-whitespace? c)
          (char=? c #\, )
          (char=? c #\[ )
          (char=? c #\] )))
    (define (loop chars acc token)
      (cond
        ((null? chars)
         (if (string=? token "")
             (reverse acc)
             (reverse (cons token acc))))
        ((is-separator? (car chars))
         (if (string=? token "")
             (loop (cdr chars) acc "")
             (loop (cdr chars) (cons token acc) "")))
        (else
         (loop (cdr chars) acc (string-append token (string (car chars)))))))
    (loop (string->list str) '() ""))

  ;; Parser: returns AST ((point name x y) ...) ((node name point-name (edges ...)) ...)
  (define (parse-lines lines)
    (let loop ((lines lines) (line-num 1) (section #f) (points '()) (nodes '()))
      (if (null? lines)
          (append (reverse points) (reverse nodes))
          (let* ((line (car lines))
                 (tokens (tokenize line line-num)))
            (cond
              ((eq? (car tokens) 'section)
               (loop (cdr lines) (+ line-num 1) (cdr tokens) points nodes))
              ((eq? section ".points")
               ;; parse point line: name: x y
               (if (< (length tokens) 3)
                   (error 'parse (format "Line ~a: point line must have 3 tokens after name" line-num))
                   (let ((name (car tokens))
                         (x (string->number (cadr tokens)))
                         (y (string->number (caddr tokens))))
                     (if (or (not x) (not y))
                         (error 'parse (format "Line ~a: invalid number in point definition" line-num))
                         (loop (cdr lines) (+ line-num 1) section
                               (cons `(point ,name ,x ,y) points)
                               nodes)))))
              ((eq? section ".nodes")
               ;; parse node line: name: point-name [edge1, edge2, ...]
               (let ((name (car tokens))
                     (pt-name (cadr tokens))
                     (edges (map string->symbol (cdr (cddr tokens)))))
                 (loop (cdr lines) (+ line-num 1) section
                       points
                       (cons `(node ,name ,pt-name ,edges) nodes))))
              (else
               (error 'parse (format "Line ~a: line outside of section or unknown section ~a" line-num section)))))))

  ;; Evaluator → builds graph from AST
  (define (eval-ast ast)
    (let* ((points (filter (lambda (x) (eq? (car x) 'point)) ast))
           (nodes-raw (filter (lambda (x) (eq? (car x) 'node)) ast))
           (point-table
            (fold-left (lambda (tbl entry)
                         (let ((name (cadr entry))
                               (pt (make-point (caddr entry) (cadddr entry))))
                           (cons (cons name pt) tbl)))
                       '() points))
           (node-table
            (map (lambda (entry)
                   (let* ((name (cadr entry))
                          (pt-name (caddr entry))
                          (edge-names (cadddr entry))
                          (pt (cdr (assoc pt-name point-table))))
                     (cons name (make-node pt edge-names))))
                 nodes-raw)))
      ;; Resolve edges to node references
      (map (lambda (pair)
             (let ((name (car pair))
                   (nd (cdr pair)))
               (make-node
                 (node-value nd)
                 (map (lambda (ename)
                        (cdr (assoc ename node-table)))
                      (node-edges nd)))))
           node-table)))

  ;; Main API to parse file
  (define (parse-graph-from-file filename)
    (call-with-input-file filename
      (lambda (port)
        (eval-ast (parse-lines (port->lines port))))))

  ;; Reads all lines from port
  (define (port->lines port)
    (let loop ((acc '()))
      (let ((line (read-line port 'any)))
        (if (eof-object? line)
            (reverse acc)
            (loop (cons line acc))))))

  ;; Serializer (reuse node and point types from algos)
  (define (serialize-graph nodes)
    (let* ((point->name
             (let loop ((nodes nodes) (counter 0) (acc '()))
               (if (null? nodes)
                   acc
                   (let* ((pt (node-value (car nodes)))
                          (existing (assoc pt acc)))
                     (if existing
                         (loop (cdr nodes) counter acc)
                         (loop (cdr nodes) (+ counter 1)
                               (cons (cons pt (string->symbol (string-append "p" (number->string counter))))
                                     acc))))))
           (node->name
             (let loop ((nodes nodes) (counter 0) (acc '()))
               (if (null? nodes)
                   acc
                   (loop (cdr nodes) (+ counter 1)
                         (cons (cons (car nodes) (string->symbol (string-append "n" (number->string counter))))
                               acc)))))
           (get-point-name (lambda (pt) (cdr (assoc pt point->name))))
           (get-node-name (lambda (nd) (cdr (assoc nd node->name)))))

      (string-append
       "-- points\n"
       (string-join
        (map (lambda (pair)
               (let* ((pt (car pair))
                      (name (cdr pair)))
                 (format "~a: ~a ~a"
                         name (point-x pt) (point-y pt))))
             point->name)
        "\n")
       "\n\n-- nodes\n"
       (string-join
        (map (lambda (nd)
               (let* ((name (get-node-name nd))
                      (pt-name (get-point-name (node-value nd)))
                      (edge-names
                       (map get-node-name (node-edges nd))))
                 (format "~a: ~a [~a]"
                         name pt-name
                         (string-join (map symbol->string edge-names) ", "))))
             nodes)
        "\n")))))))
