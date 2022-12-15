(add-to-load-path ".")
(use-modules (aoc))

(define (parse-range range)
  (pipe>
    (string-split range #\-)
    (map string->number)
    (apply make-range)))

(define (parse-line line)
  (pipe>
    (string-split line #\,)
    (map parse-range)))

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
