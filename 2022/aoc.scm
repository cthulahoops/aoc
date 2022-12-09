(define-module (aoc)
  #:export (gather-list read-lines sum pipe> read-blocks read-block zip-lists minimum maximum display-lines character->number make-counter counter-get counter-add counter->list))

(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 vlist))
(use-modules (srfi srfi-1))

(define (gather-list get-next end?)
  (let loop ((item (get-next)) (items (list)))
    (if (end? item)
        (reverse items)
        (loop (get-next) (cons item items)))))
(define (read-lines) (gather-list read-line eof-object?))

(define (read-blocks) (gather-list read-block null?))
(define (read-block) (gather-list read-line block-end?))
(define (block-end? line) (or (eof-object? line) (string-null? line)))

(define (sum items) (fold + 0 items))

(define (append-item final-element items) (reverse (cons final-element (reverse items))))

(define-macro (pipe> value . pipeline)
  (if
    (null? pipeline)
    value
    (let*
      ((next-item (car pipeline))
       (rest-of-pipeline (cdr pipeline)))
      `(pipe> ,(append-item value next-item) . ,rest-of-pipeline)))
      )

(define (character->number c) (string->number (list->string (list c))))

;; (define (zip-lists args) (apply map-in-order (cons list (map string->list args))))
(define (zip-lists args) (apply map-in-order (cons list args)))
(define (minimum items) (car (sort items <)))
(define (maximum items) (car (sort items >)))

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
