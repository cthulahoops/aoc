(use-modules (ice-9 textual-ports))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 format))
(use-modules (srfi srfi-1))

(define (read-blocks) (gather-list read-block null?))
(define (read-block) (map string->number (gather-list read-line block-end?)))
(define (block-end? line) (or (eof-object? line) (string-null? line)))

(define (gather-list get-next end?)
  (let loop ((item (get-next)) (items (list)))
    (if (end? item)
        (reverse items)
        (loop (get-next) (cons item items)))))

(define (sum items) (fold + 0 items))
(define (maximum items) (fold max 0 items))

(define blocks (with-input-from-file "input/1" read-blocks))

(define sorted-totals (sort (map sum blocks) >))

(format #t "Part 1: ~d\n" (car sorted-totals))
(format #t "Part 2: ~d\n" (sum (take sorted-totals 3)))
