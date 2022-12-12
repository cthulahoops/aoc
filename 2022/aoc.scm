(define-module (aoc)
  #:export (gather-list read-lines sum pipe> read-blocks read-block zip-lists
            minimum maximum replicate display-lines character->number count-unique
            apply-n-times make-counter counter-get counter-add counter->list enumerate
            range chunk iterate
            alist->hash-table
            flip
            make-point point-x point-y point? set-point-x set-point-y point-+))

(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-1))
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

(define (character->number c) (string->number (list->string (list c))))

;; (define (zip-lists args) (apply map-in-order (cons list (map string->list args))))
(define (zip-lists args) (apply map-in-order (cons list args)))
(define (minimum items) (car (sort items <)))
(define (maximum items) (car (sort items >)))
(define (replicate n v) (if (= n 0) (list) (cons v (replicate (- n 1) v))))

(define (enumerate items)
  (let loop ((count 1) (items items) (result (list)))
    (if (null? items)
        (reverse result)
        (loop (+ 1 count) (cdr items) (cons (cons count (car items)) result)))))

(define (chunk size items)
  (let loop ((items items) (result (list)))
    (if (null? items)
        (reverse result)
        (loop (drop items size) (cons (take items size) result)))))

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
  (let loop ((n n) (result (list)))
    (if
      (> n m)
      (reverse result)
      (loop (+ n 1) (cons n result)))))


(define-immutable-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x)
  (y point-y set-point-y))

(define (point-+ p1 p2) (make-point (+ (point-x p1) (point-x p2)) (+ (point-y p1) (point-y p2))))

(define (alist->hash-table alist)
  (let ((table (make-hash-table)))
    (map (lambda (pair) (hash-set! table (car pair) (cdr pair))) alist)
    table
    ))

(define (flip f) (lambda (x y) (f y x)))
