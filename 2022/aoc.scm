(define-module (aoc)
  #:export (gather-list read-lines sum pipe> define-match define-match* read-blocks read-block zip-lists
            minimum minimum-by maximum maximum-by replicate display-lines character->number count-unique
            apply-n-times make-counter counter-get counter-add counter->list enumerate
            range chunk iterate
            alist->hash-table
            flip partial count-where
            sign
            <point> make-point point? point-x point-y set-point-x set-point-y point+ point- point-sign
            <point3> make-point3 point3? point3-x point3-y point3-z set-point3-x set-point3-y set-point3-z point3+ point3- point3*
            cross-product point->point3
            make-range range? range-start range-end range-overlaps? range-before? range-contains? range-length))

(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 vlist))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))

(define (gather-list get-next end?)
  (let loop ((item (get-next)) (items (list)))
    (if (end? item)
        (reverse items)
        (loop (get-next) (cons item items)))))
(define (read-lines) (gather-list read-line eof-object?))

(define (read-blocks) (gather-list read-block null?))
(define (read-block) (gather-list read-line block-end?))
(define (block-end? line) (or (eof-object? line) (string-null? line)))

(define (sum items) (apply + items))

(define-macro (pipe> value . pipeline)
  (if
    (null? pipeline)
    value
    (let*
      ((next-item (car pipeline))
       (rest-of-pipeline (cdr pipeline)))
      `(pipe> ,(append next-item (list value)) . ,rest-of-pipeline)))
      )

(define-macro (define-match* name . clauses)
    `(define ,name (match-lambda* . ,clauses)))

(define-macro (define-match name . clauses)
    `(define ,name (match-lambda . ,clauses)))

(define (character->number c) (string->number (list->string (list c))))

;; (define (zip-lists args) (apply map-in-order (cons list (map string->list args))))
(define (zip-lists args) (apply map-in-order (cons list args)))
(define (minimum items) (apply min items))
(define (maximum items) (apply max items))
(define (maximum-by f items) (extreme-by > f items))
(define (minimum-by f items) (extreme-by < f items))

(define (extreme-by compare f items)
  (let loop ((best (car items)) (best-value (f (car items))) (rest (cdr items)))
    (if (null? rest)
        best
        (let ((new-best (f (car rest))))
          (if (compare new-best best-value)
              (loop (car rest) new-best (cdr rest))
              (loop best best-value (cdr rest)))))))

(define (replicate n v) (if (= n 0) (list) (cons v (replicate (- n 1) v))))

(define (enumerate items)
  (map cons (range 1 (+ (length items) 1)) items))

(define (chunk size items)
  (let loop ((items items) (result (list)))
    (cond ((null? items) (reverse result))
          ((< (length items) size) (loop '() (cons items result)))
          (else (loop (drop items size) (cons (take items size) result))))))

(define (apply-n-times n f v)
  (let loop ((n n) (v v))
    (if (= n 0)
        v
        (loop (- n 1) (f v)))))

(define (iterate f init items)
  (reverse (fold (lambda (item acc) (cons (f item (car acc)) acc)) (list init) items)))

(define (display-lines lines)
  (map (lambda (x) (begin (display x) (newline))) lines)
  (newline)
  lines)

; Counter
(define (make-counter) (alist->vhash (list)))
(define (counter-add key value counter)
  (let* ((old-count (counter-get key counter))
         (new-count (+ old-count value)))
         (vhash-cons key new-count (vhash-delete key counter))))
(define (counter-get key counter)
  (cdr (or (vhash-assoc key counter) (cons key 0))))
(define counter->list vlist->list)

(define (count-unique items)
  (pipe>
    (fold (lambda (x counter) (counter-add x 1 counter)) (make-counter) items)
    (counter->list)
    (length)))

(define (range n m)
  (unfold (lambda (p) (= p m)) identity (lambda (x) (+ 1 x)) n))

(define (sign value)
  (cond ((negative? value) -1)
        ((positive? value) 1)
        (else value)))

(define-immutable-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x)
  (y point-y set-point-y))


(define (point+ p1 p2) (make-point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))
(define (point- p1 p2) (make-point (- (point-x p1) (point-x p2)) (- (point-y p1) (point-y p2))))
(define (point-sign p1) (make-point (sign (point-x p1)) (sign (point-y p1))))

(define (alist->hash-table alist)
  (let ((table (make-hash-table)))
    (map (lambda (pair) (hash-set! table (car pair) (cdr pair))) alist)
    table
    ))

(define (flip f) (lambda (x y) (f y x)))
(define (partial f . args1) (lambda args2 (apply f (append args1 args2))))
(define count-where (compose length filter))


(define-record-type <range>
  (make-range start end)
  range?
  (start range-start)
  (end range-end))

(define (range-contains? a b) (and (<= (range-start a) (range-start b)) (<= (range-end b) (range-end a))))
(define (range-before? a b) (< (range-end a) (range-start b)))
(define (range-overlaps? a b) (not (or (range-before? a b) (range-before? b a))))
(define (range-length r) (+ 1 (- (range-end r) (range-start r))))

(define-immutable-record-type <point3>
  (make-point3 x y z)
  point3?
  (x point3-x set-point3-x)
  (y point3-y set-point3-y)
  (z point3-z set-point3-z))

(define (point3+ . points) (make-point3 (apply + (map point3-x points)) (apply + (map point3-y points)) (apply + (map point3-z points))))
(define (point3- p1 p2) (make-point3 (- (point3-x p1) (point3-x p2)) (- (point3-y p1) (point3-y p2)) (- (point3-z p1) (point3-z p2))))
(define (point3* c p2) (make-point3 (* c (point3-x p2)) (* c (point3-y p2)) (* c (point3-z p2))))
(define-match* cross-product
  ((($ <point3> x1 y1 z1) ($ <point3> x2 y2 z2)) (make-point3 (- (* y1 z2) (* y2 z1)) (- (* z1 x2) (* z2 x1)) (- (* x1 y2) (* x2 y1)))))
(define-match point->point3 (($ <point> x y) (make-point3 x y 0)))
