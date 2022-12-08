(use-modules (ice-9 textual-ports))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))
(use-modules (aoc))

; Input parsing:
(define (tree-line-heights line)
  (pipe>
    (string->list line)
    (map character->number)))

(define (tree-heights)
    (pipe>
      (read-lines)
      (map tree-line-heights)))

; Part 1:
(define (visible-from-side trees)
  (let loop ((visible (list)) (blocking -1) (trees trees))
    (if (null? trees)
        (reverse visible)
        (if (> (car trees) blocking)
            (loop (cons #t visible) (car trees) (cdr trees))
            (loop (cons #f visible) blocking (cdr trees))))))

(define (|| x y) (or x y)) ; Or is a special form and can't be used as a function.

(define (visible-from-either-end trees)
  (map-in-order || (visible-from-side trees) (apply-reversed visible-from-side trees)))

(define (visible-from-left-right forest)
  (map visible-from-either-end forest))

(define (visible-from-top-bottom forest)
  (apply-rotated visible-from-left-right forest))

(define (visibility forest)
  (merge-grids || (visible-from-left-right forest) (visible-from-top-bottom forest)))

(define (count-true items) (sum (map (lambda (x) (if x 1 0)) items)))
(define (count-visible visibility) (sum (map count-true visibility)))

; Part 2
(define (trees-seen-from-tree tree trees)
  (length (take-until (lambda (x) (>= x tree)) trees)))

(define (trees-seen trees)
  (map (lambda (viewing-point) (trees-seen-from-tree (car viewing-point) (cdr viewing-point))) (tails trees)))

(define (scenic-either-end trees)
  (map-in-order * (trees-seen trees) (apply-reversed trees-seen trees)))

(define (scenic-left-right forest)
  (map scenic-either-end forest))

(define (scenic-top-bottom forest)
  (apply-rotated scenic-left-right forest))

(define (scenic forest)
  (merge-grids * (scenic-left-right forest) (scenic-top-bottom forest)))

; Helper functions:
(define (apply-rotated f grid) (zip-lists (f (zip-lists grid))))
(define (apply-reversed f line) (reverse (f (reverse line))))

(define (merge-grids f a b) (map-in-order (lambda (x y) (map-in-order f x y)) a b))
(define (tails lst)
  (if
    (null? lst)
    (list)
    (cons lst (tails (cdr lst)))))

(define (take-until f lst)
  (if
    (null? lst)
    lst
    (if (f (car lst))
        (list (car lst))
        (cons (car lst) (take-until f (cdr lst))))))

(define (reduce-grid f grid) (f (map f grid)))
(define (map-grid f grid) (map (lambda (line) (map f line)) grid))

; Main
(define (main args)
    (let* (
           (forest (tree-heights))
           (visibility (visibility forest))
           (scenic (scenic forest)))
      (format #t "Part 1: ~s\n" (count-visible visibility))
      (format #t "Part 2: ~s\n" (reduce-grid maximum scenic))))

(define (part1)
   (pipe>
     (tree-heights)
     (visibility)
     (count-visible)))

(define (part2)
   (pipe> (tree-heights) (scenic) (reduce-grid maximum)))
