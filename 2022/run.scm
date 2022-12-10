#! /usr/bin/guile \
-e main -s
!#
(add-to-load-path ".")

(define (script day) (string-append "day" day ".scm"))
(define (input day suffix) (string-append "input/" day suffix))

(define (with-input day suffix f)
  (with-input-from-file (input day suffix) f))

(define (main args)
  (let* (
      (day (cadr args))
      (input-suffix (if (null? (cddr args)) "" (caddr args)))
      (script (script day)))
    (load script)
    (format #t "Part 1: ~a\n" (with-input day input-suffix part1))
    (format #t "Part 2: ~a\n" (with-input day input-suffix part2))))
