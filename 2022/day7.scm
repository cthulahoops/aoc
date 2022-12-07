(add-to-load-path ".")
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 format))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-11))
(use-modules (ice-9 vlist))
(use-modules (aoc))

(define-record-type <file>
  (make-file name size)
  file?
  (name file-name)
  (size file-size))

(define-record-type <dir>
  (make-dir name)
  dir?
  (name dir-name))

(define-record-type <vm>
  (make-vm pwd fs)
  vm?
  (pwd vm-pwd)
  (fs vm-fs))

(define (parse-command cmd) (string-split (drop-$ cmd) #\space))
(define (drop-$ cmd) (substring cmd 2))

(define (parse-entry entry)
  (let* ((words (string-split entry #\space))
         (size (car words))
         (name (cadr words)))
    (if (equal? size "dir")
      (make-dir name)
      (make-file name (string->number size)))))

(define (get-command lines)
  (let-values (((response rest) (span (lambda (x) (not (string-prefix? "$ " x))) (cdr lines))))
    (values (parse-command (car lines)) response rest)))

(define (split-command-responses lines)
  (let loop ((lines lines) (result (list)))
    (if (null? lines)
      (reverse result)
      (let-values (((command response rest) (get-command lines)))
        (loop rest (cons (cons command response) result))))))

(define (cd? cmd) (equal? (car cmd) "cd"))
(define (cd-arg cmd) (cadr cmd))

(define (cd-destination arg pwd)
  (cond ((equal? "/" arg) (list))
        ((equal? ".." arg) (cdr pwd))
        (else (cons arg pwd))))

(define (handle-command command-response vm)
  (let (
      (command (car command-response))
      (response (cdr command-response))
      (pwd (vm-pwd vm))
      (fs (vm-fs vm)))
    (if (cd? command)
      (make-vm (cd-destination (cd-arg command) pwd) fs)
      (make-vm pwd (append (map (lambda (x) (cons pwd (parse-entry x))) response) fs))
      )))

(define (run-vm-with-history command-history)
  (vm-fs (fold handle-command (make-vm (list) (list)) command-history)))

(define (make-counter) (alist->vhash (list)))
(define (count key value counter)
  (let ((old-count (cdr (or (vhash-assoc key counter) (cons key 0)))))
         (vhash-cons key (+ old-count value) (vhash-delete key counter))))

(define (pwd->path pwd) (string-append "/" (string-join (reverse pwd) "/")))
(define (paths pwd)
  (if 
    (null? pwd)
    (list "/")
    (cons (pwd->path pwd) (paths (cdr pwd)))))


(define (count-entry pwd-entry counter)
  (let ((pwd (car pwd-entry)) (entry (cdr pwd-entry)))
    (if
      (file? entry)
      (fold (lambda (path counter) (count path (file-size entry) counter)) counter (paths pwd))
      counter ; Directories don't themselves have a size
      )))

(define (compute-directory-sizes vm)
  (fold count-entry (make-counter) vm))

(define (load-and-compute-sizes)
  (pipe>
     (with-input-from-file "input/7" read-lines)
     (split-command-responses)
     (run-vm-with-history)
     (compute-directory-sizes)))

(define (part1)
   (pipe>
     (load-and-compute-sizes)
     (vlist->list)
     (filter (lambda (x) (<= (cdr x) 100000)))
     (map cdr)
     (sum)))

(define (sort-by-size x) (sort x (lambda (x y) (< (cdr x) (cdr y)))))

(define (part2)
   (let* (
       (directory-sizes (load-and-compute-sizes))
       (total-used (cdr (vhash-assoc "/" directory-sizes)))
       (available-space (- 70000000 total-used))
       (needed-deletion (- 30000000 available-space)))
     (pipe>
       (filter (lambda (x) (>= (cdr x) needed-deletion)) (vlist->list directory-sizes))
       (sort-by-size)
       (car)
       (cdr))))

(format #t "Part 1: ~s\n" (part1))
(format #t "Part 2: ~s\n" (part2))
