(define-module (aoc)
  #:export (gather-list read-lines sum))

(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (srfi srfi-1))

(define (gather-list get-next end?)
  (let loop ((item (get-next)) (items (list)))
    (if (end? item)
        (reverse items)
        (loop (get-next) (cons item items)))))
(define (read-lines) (gather-list read-line eof-object?))

(define (sum items) (fold + 0 items))
