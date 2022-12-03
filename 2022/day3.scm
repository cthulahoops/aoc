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

(define (common-element-in-rucksack rucksack)
    (common-element (split-compartments rucksack)))

(define (common-element x) (car (char-set->list (apply char-set-intersection (map string->char-set x)))))

(define (read-group) (list (read-line) (read-line) (read-line)))
(define (read-groups) (gather-list read-group (lambda (x) (eof-object? (car x)))))

(define input-lines (with-input-from-file "input/3" read-lines))
(define part1 (sum (map (lambda (x) (priority (common-element-in-rucksack x))) input-lines)))
(format #t "Part 1: ~d\n" part1)

(define input-lines-2 (with-input-from-file "input/3" read-groups))
(define part2 (sum (map (lambda (x) (priority (common-element x))) input-lines-2)))
(format #t "Part 1: ~d\n" part2)
