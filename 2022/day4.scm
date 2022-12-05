(add-to-load-path ".")
(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-9))
(use-modules (aoc))

(define-record-type <range>
  (make-range start end)
  range?
  (start range-start)
  (end range-end))

(define (parse-range range)
  (pipe>
    (string-split range #\-)
    (map string->number)
    (apply make-range)))

(define (parse-line line)
  (pipe>
    (string-split line #\,)
    (map parse-range)))

(define (range-contains? a b) (and (<= (range-start a) (range-start b)) (<= (range-end b) (range-end a))))
(define (range-before? a b) (< (range-end a) (range-start b)))
(define (range-overlaps? a b) (not (or (range-before? a b) (range-before? b a))))

(define (part1)
  (pipe>
    (with-input-from-file "input/4" read-lines)
    (map parse-line)
    (filter (lambda (x) (or (range-contains? (car x) (cadr x)) (range-contains? (cadr x) (car x)))))
    (length)))

(define (part2)
  (pipe>
    (with-input-from-file "input/4" read-lines)
    (map parse-line)
    (filter (lambda (x) (apply range-overlaps? x)))
    (length)))

(format #t "Part 1: ~d\n" (part1))
(format #t "Part 2: ~d\n" (part2))
