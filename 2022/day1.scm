(add-to-load-path ".")
(use-modules (ice-9 format))
(use-modules (srfi srfi-1))
(use-modules (aoc))

(define (keep-biggest new items) (cdr (sort (cons new items) <)))

(define (read-blocks) (gather-list read-block null?))
(define (read-block) (map string->number (gather-list read-line block-end?)))
(define (block-end? line) (or (eof-object? line) (string-null? line)))

(define (maximum items) (fold max 0 items))

(define blocks (with-input-from-file "input/1" read-blocks))

(display (sum (fold keep-biggest '(0 0 0) (map sum blocks))))

(define sorted-totals (sort (map sum blocks) >))

(format #t "Part 1: ~d\n" (car sorted-totals))
(format #t "Part 2: ~d\n" (sum (take sorted-totals 3)))
