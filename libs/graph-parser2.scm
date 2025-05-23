(import (xitomatl AS-match) (algos) (srfi :1))

(define-syntax ->
  (syntax-rules ()
    ((_ x) x)
    ((_ x f) (f x))
    ((_ x f1 f2 ...) (-> (f1 x) f2 ...))))

(define-syntax ->!
  (syntax-rules ()
    ((_ x) x)
    ((_ x (f . args) more ...)
         (begin
           (f x . args)
           (->! x more ...)))
    ((_ x f1 f2 ...)
     (begin
       (f1 x)
       (->! x f2 ...)))))

(define-record-type (lexer make-lexer is-lexer?) 
  (fields
    (immutable input-text lexer-input)
    (mutable position lexer-pos lexer-pos!)
    (mutable tokens lexer-tok lexer-tok!)))

(define (lexer-next! lexer)
  (lexer-pos! lexer (+ (lexer-pos lexer) 1)))

(define (lexer-add-tok! lexer tok)
  (lexer-tok! lexer (cons tok (lexer-tok lexer))))

(define (lexer-last-ident lexer)
  (car (lexer-tok lexer)))

(define (lexer-cur lexer)
  (let ((pos (lexer-pos lexer))
        (len (string-length (lexer-input lexer))))
    (if (>= pos len)
        #\x00
        (string-ref (lexer-input lexer) pos))))

(define (lexer-peek lexer)
  (let ((pos (+ 1 (lexer-pos lexer)))
        (len (string-length (lexer-input lexer))))
    (if (>= pos len)
        #\x00
        (string-ref (lexer-input lexer) pos))))

(define (lexer-skip-whitespace! lexer)
  (let loop ()
    (when (char-whitespace? (lexer-cur lexer))
      (lexer-next! lexer)
      (loop))))

(define (lexer-expect! lexer f)
  (if (f (lexer-cur lexer))
    (lexer-next! lexer)
    (error 'lexer-expect "unexpected character" (lexer-tok lexer))))

(define (lexer-tokenstream lexer)
  (reverse (lexer-tok lexer)))

(define (lexer-read-ident! lexer)
  (define (read-id! ls)
    (let ((cur-char (lexer-cur lexer)))
      (if (not (or (char-alphabetic? cur-char) (char-numeric? cur-char)))
        (list->string (reverse ls))
        (begin
          (lexer-next! lexer)
          (read-id! (cons cur-char ls))))))
  (lexer-add-tok! lexer `(identifier ,(read-id! '()))))

(define (lexer-read-number! lexer)
  (let loop ((ls '())
             (cur-char (lexer-cur lexer)))
    (if (not (char-numeric? cur-char))
      (lexer-add-tok! lexer `(number ,(string->number (list->string (reverse ls)))))
      (begin
        (lexer-next! lexer)
        (loop (cons cur-char ls) (lexer-cur lexer))))))

(define (lexer-read-points! lexer)
  (let loop ()
    (when (char-alphabetic? (lexer-cur lexer))
      (->! lexer
           lexer-read-ident!
           (lexer-expect! colon?)
           (lexer-add-tok! '(colon ()))
           lexer-skip-whitespace!
           lexer-read-number!
           lexer-skip-whitespace!
           lexer-read-number!
           lexer-skip-whitespace!)
      (loop))))

(define (lexer-read-edges! lexer)
  (let loop ((cur-char (lexer-cur lexer)))
    (cond
      ((char=? #\] cur-char) 
         (begin
           (lexer-next!    lexer)
           (lexer-add-tok! lexer '(close-bracket ()))))
       (else                  
         (begin
           (lexer-read-ident!      lexer)
           (lexer-skip-whitespace! lexer)
           (loop (lexer-cur lexer)))))))

(define (lexer-read-nodes! lexer)
  (define (open-brac? c) (char=? c #\[))
  (let loop ()
    (when (char-alphabetic? (lexer-cur lexer))
      (->! lexer
           lexer-read-ident!
           lexer-skip-whitespace!
           (lexer-expect! colon?)
           (lexer-add-tok! '(colon ()))
           lexer-skip-whitespace!
           lexer-read-ident!
           lexer-skip-whitespace!
           (lexer-expect! open-brac?)
           (lexer-add-tok! '(open-bracket ()))
           lexer-read-edges!
           lexer-skip-whitespace!)
      (loop))))

(define block-types
  `(("nodes" . ,lexer-read-nodes!)
    ("points" . ,lexer-read-points!)))

(define (lexer-read-block! lexer)
  (define (dash c) (char=? c #\-))
  (->! lexer
    lexer-skip-whitespace! 
    (lexer-expect! dash)
    (lexer-expect! dash)
    (lexer-add-tok! '(open-block ()))
    lexer-skip-whitespace!
    lexer-read-ident!
    lexer-skip-whitespace!)
  ((cdr (assoc (cadr (lexer-last-ident lexer)) block-types)) lexer))

(define (lex-graph input-text) 
  (define lexer (make-lexer input-text 0 '()))
  (let loop ()
    (if (char=? (lexer-cur lexer) #\x00)
      (lexer-tokenstream lexer)
      (begin
        (lexer-read-block! lexer)
        (loop)))))
(define (colon? c) (char=? c #\:))

(define-record-type naive-tree
  (fields
    (mutable points tree-points tree-points!)
    (mutable nodes  tree-nodes  tree-nodes! )))

(define-record-type parsed-point
  (fields name point))

(define-record-type parsed-node
  (fields name point-ref edge-names))

(define (parse-points points)
  (let loop ((points points)
             (acc '()))
    (match points
      [((identifier point-name) (colon ()) (number x) (number y) . rest)
       (loop rest (cons (make-parsed-point point-name (make-point x y)) acc))]
      [_ (list points acc)])))

(define (parse-edges edges)
  (let loop ((edges edges)
             (acc '()))
    (match edges
      [(('open-bracket '()) . rest)
        (loop rest acc)]
      [(('identifier idname) . rest)
        (loop rest (cons idname acc))]
      [(('close-bracket '()) . rest)
        (list rest acc)]
      [(irr . rest) (error 'parse-edges "unexpected token in edges" irr)]
      [_ (list rest acc)])))

(define (parse-nodes nodes)
  (let loop ((nodes nodes)
             (acc '()))
    (match nodes
      [(('identifier idname) ('colon '()) ('identifier pname) . edges)
       (match-let [((cont edges) (parse-edges edges))]
         (loop cont (cons (make-parsed-node idname pname edges) acc)))]
      [_ (list nodes acc)])))

(define (parse-lexemes lexemes)
  (define tree (make-naive-tree '() '()))
  (let loop ((lexemes lexemes))
    (match lexemes
      [(('open-block ()) ('identifier "nodes") . connections)
       (match-let [((cont p-nodes) (parse-nodes connections))]
         (tree-nodes! tree p-nodes)
         (loop cont))]
      [(('open-block ()) ('identifier "points") . points) 
       (match-let [((cont p-points) (parse-points points))]
         (tree-points! tree p-points)
         (loop cont))]
      [(irr . rest) (error 'parse-lexemes "unexpected input" irr)]
      [() tree])))

(define-record-type graph
  (fields nodes))

(define (resolve-graph naive)
  ;; Build an assoc list of point-name -> point
  (define point-alist
    (map (lambda (pp)
           (cons (parsed-point-name pp)
                 (parsed-point-point pp)))
         (tree-points naive)))

  ;; Build assoc list of node-name -> partial node (without edges)
  (define node-alist
    (map (lambda (pn)
           (let* ((point-name (parsed-node-point-ref pn))
                  (point-pair (assoc point-name point-alist)))
             (unless point-pair
               (error 'resolve-graph
                      (format "Node ~a refers to unknown point ~a"
                              (parsed-node-name pn)
                              point-name)))
             (cons (parsed-node-name pn)
                   (make-node (cdr point-pair)
                              '())))) ; edges added later
         (tree-nodes naive)))

  ;; Now resolve edge names to actual node objects
  (define resolved-nodes
    (map (lambda (pn)
           (let* ((self-pair (assoc (parsed-node-name pn) node-alist))
                  (self (cdr self-pair))
                  (edges (map (lambda (edge-name)
                                (let ((target-pair (assoc edge-name node-alist)))
                                  (unless target-pair
                                    (error 'resolve-graph
                                           (format "Edge to unknown node ~a"
                                                   edge-name)))
                                  (cdr target-pair)))
                              (parsed-node-edge-names pn))))
             (make-node (node-value self)
                        edges)))
         (tree-nodes naive)))

  ;; Final graph
  (make-graph resolved-nodes))


(display (resolve-graph (parse-lexemes (lex-graph "
-- points
p1: 10 10
p2: 11 10
p3: 10 11
p4: 11 12

-- nodes
A: p1 [B C]
B: p3 [C]
C: p3 [D]
D: p4 []
"))))

(newline)
(newline)
(newline)

(display (-> "
              -- nodes
              A: p1 [B C]
              B: p3 [C]
              C: p3 [D]
              D: p4 []

              -- points
              p1: 10 10
              p2: 11 10
              p3: 10 11
              p4: 11 12
              "
              lex-graph
              parse-lexemes
              resolve-graph))

; (define lexer 'lexer)

; (display
;   (expand `(->! lexer
;     lexer-read-ident!
;     (lexer-expect! colon?)
;     (lexer-add-tok! '(colon ()))
;     lexer-skip-whitespace!
;     lexer-read-number!
;     lexer-skip-whitespace!
;     lexer-read-number!
;     lexer-skip-whitespace!)))

