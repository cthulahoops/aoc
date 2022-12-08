(add-to-load-path ".")
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 format))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-11))
(use-modules (ice-9 vlist))
(use-modules (ice-9 match))
(use-modules (aoc))

(define (tree-line-heights line)
  (pipe>
    (string->list line)
    (map (lambda (c) (string->number (list->string (list c)))))))

(define (tree-heights)
    (pipe>
      (with-input-from-file "input/8" read-lines)
      (map tree-line-heights)))

(define (visible-from-side trees)
  (let loop ((visible (list)) (blocking -1) (trees trees))
    (if (null? trees)
        (reverse visible)
        (if (> (car trees) blocking)
            (loop (cons #t visible) (car trees) (cdr trees))
            (loop (cons #f visible) blocking (cdr trees))))))

(define (trees-seen trees)
  (map (lambda (viewing-point) (trees-seen-from-tree (car viewing-point) (cdr viewing-point))) (tails trees)))

(define (tails lst)
  (if
    (null? lst)
    (list)
    (cons lst (tails (cdr lst)))))

(define (trees-seen-from-tree tree trees)
  (length (take-until (lambda (x) (>= x tree)) trees)))

(define (take-until f lst)
  (if
    (null? lst)
    lst
    (if (f (car lst))
        (list (car lst))
        (cons (car lst) (take-until f (cdr lst))))))

(define (zip-or list1 list2) 
  (map-in-order (lambda (x y) (or x y)) list1 list2))

(define (visible-from-either-end trees)
  (zip-or (visible-from-side trees) (reverse (visible-from-side (reverse trees)))))

(define (visible-from-left-right forest)
  (map visible-from-either-end forest))

(define (visible-from-top-bottom forest)
  (pipe> 
    (zip-lists forest)
    (visible-from-left-right)
    (zip-lists)))

(define (merge-visibility visibility1 visibility2)
  (map-in-order zip-or visibility1 visibility2))

(define (count-true items) (sum (map (lambda (x) (if x 1 0)) items)))
(define (count-visible visibility) (sum (map count-true visibility)))

; Part 2
(define (zip-* list1 list2) 
  (map-in-order * list1 list2))

(define (scenic-either-end trees)
  (zip-* (trees-seen trees) (reverse (trees-seen (reverse trees)))))

(define (scenic-left-right forest)
  (map scenic-either-end forest))

(define (scenic-top-bottom forest)
  (pipe> 
    (zip-lists forest)
    (scenic-left-right)
    (zip-lists)))

(define (merge-scenic scenic1 scenic2)
  (map-in-order zip-* scenic1 scenic2))

(define (display-lines lines)
  (map (lambda (x) (begin (display x) (newline))) lines)
  (newline)
  lines)

(define (main args)
    (let* (
           (forest (tree-heights))
           (left-right (visible-from-left-right forest))
           (top-bottom (visible-from-top-bottom forest))
           (visibility (merge-visibility left-right top-bottom))
           (scenic-left-right (scenic-left-right forest))
           (scenic-top-bottom (scenic-top-bottom forest))
           (scenic (merge-scenic scenic-left-right scenic-top-bottom)))
      (display-lines forest)
      (display-lines left-right)
      (display-lines top-bottom)
      (display-lines visibility)
      (format #t "Part 1: ~s\n\n" (count-visible visibility))
      (display-lines scenic)
      (format #t "Part 2: ~s\n" (maximum (map maximum scenic)))
      )
    (newline))
