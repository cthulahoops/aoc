#! /usr/bin/guile \
-e main -s
!#
(add-to-load-path ".")

(define (script day) (string-append "day" day ".scm"))
(define (input day) (string-append "input/" day))

(define (main args)
  (let* (
      (day (cadr args))
      (script (script day))
      (input (input day)))
    (load script)
    (format #t "Part 1: ~s\n" (with-input-from-file input part1))
    (format #t "Part 2: ~s\n" (with-input-from-file input part2))))
