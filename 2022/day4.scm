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
    (read-lines)
    (map parse-line)
    (filter (lambda (x) (or (range-contains? (car x) (cadr x)) (range-contains? (cadr x) (car x)))))
    (length)))

(define (part2)
  (pipe>
    (read-lines)
    (map parse-line)
    (filter (lambda (x) (apply range-overlaps? x)))
    (length)))
