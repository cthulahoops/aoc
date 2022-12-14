(use-modules (ice-9 rdelim))
(use-modules (aoc))

(define (priority char)
  (if (char-lower-case? char)
      (+ 1 (- (char->integer char) (char->integer #\a)))
      (+ 27 (- (char->integer char) (char->integer #\A)))))

(define (split-compartments str)
  (let ((mid (/ (string-length str) 2)))
    (list (substring str 0 mid) (substring str mid))))

(define (common-element x) (car (char-set->list (apply char-set-intersection (map string->char-set x)))))
(define common-element-in-rucksack (compose common-element split-compartments))

(define (read-group) (list (read-line) (read-line) (read-line)))
(define (read-groups) (gather-list read-group (compose eof-object? car)))

(define (part1)
  (pipe>
    (read-lines)
    (map common-element-in-rucksack)
    (map priority)
    (sum)))

(define (part2)
  (pipe>
    (read-groups)
    (map common-element)
    (map priority)
    (sum)))
