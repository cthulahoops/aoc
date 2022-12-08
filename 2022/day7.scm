(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-11))
(use-modules (ice-9 vlist))
(use-modules (ice-9 match))
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

(define-immutable-record-type <vm>
  (make-vm pwd fs)
  vm?
  (pwd vm-pwd set-vm-pwd)
  (fs vm-fs set-vm-fs))

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

(define (cd-destination arg pwd)
  (match arg ("/" (list))
             (".." (cdr pwd))
             (dir (cons arg pwd))))

(define (response->pwd-entries pwd response)
  (map (lambda (x) (cons pwd (parse-entry x))) response))

(define (handle-command command-response vm)
  (let (
      (command (car command-response))
      (response (cdr command-response))
      (pwd (vm-pwd vm))
      (fs (vm-fs vm)))
    (match (car command)
      ("cd" (set-vm-pwd vm (cd-destination (cadr command) pwd)))
      ("ls" (set-vm-fs vm (append (response->pwd-entries pwd response) fs))))))

(define (run-vm-with-history command-history)
  (vm-fs (fold handle-command (make-vm (list) (list)) command-history)))

(define (make-counter) (alist->vhash (list)))
(define (count key value counter)
  (let* ((old-count (counter-get key counter))
         (new-count (+ old-count value)))
         (vhash-cons key new-count (vhash-delete key counter))))
(define (counter-get key counter)
  (cdr (or (vhash-assoc key counter) (cons key 0))))
(define counter->list vlist->list)

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
     (read-lines)
     (split-command-responses)
     (run-vm-with-history)
     (compute-directory-sizes)))

(define (part1)
   (pipe>
     (load-and-compute-sizes)
     (counter->list)
     (map directory-size)
     (filter (lambda (x) (<= x 100000)))
     (sum)))

(define directory-size cdr)

(define (part2)
   (let* (
       (directory-sizes (load-and-compute-sizes))
       (total-used (counter-get  "/" directory-sizes))
       (available-space (- 70000000 total-used))
       (needed-deletion (- 30000000 available-space)))
     (pipe>
       (counter->list directory-sizes)
       (map directory-size)
       (filter (lambda (x) (>= x needed-deletion)) )
       (minimum)
       )))
