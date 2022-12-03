(add-to-load-path ".")
(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))
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
  (let ((input-lines (with-input-from-file "input/3" read-lines)))
    (sum (map (compose priority common-element-in-rucksack) input-lines))))

(define (part2)
  (let ((input-lines (with-input-from-file "input/3" read-groups)))
    (sum (map (compose priority common-element) input-lines))))

(format #t "Part 1: ~d\n" (part1))
(format #t "Part 2: ~d\n" (part2))
