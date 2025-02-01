#!/usr/bin/csi -ss

(import (chicken file posix)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken process-context posix)
        (chicken sort)
        srfi-1
        srfi-13
        srfi-14
        srfi-69)

(define-record proc
  cmd pid ppid uid args)

(define (escape cmd)
  (reverse-list->string
   (string-fold
    (lambda (c lst)
      (cond
       ((char=? c #\\) (cons #\\ (cons #\\ lst)))
       ((and (char>? c #\space) (char<=? c #\~)) (cons c lst))
       (else (append-reverse
              (string->list (format "\\~3,48o" (char->integer c))) lst))))
    '() cmd)))

(define (read-args)
   (let loop ()
     (if (eof-object? (peek-char))
         '()
         (let ((arg (read-token (lambda (c) (not (char=? c #\nul))))))
           (read-char)
           (cons arg (loop))))))

(define (build-proc pid)
  (let* ((dir (string-append "/proc/" (number->string pid)))
         (uid (file-owner dir))
         (args (with-input-from-file
                   (string-append dir "/cmdline") read-args))
         (proc-stat (with-input-from-file
                        (string-append dir "/stat") read-line))
         (lparen (string-index proc-stat #\( ))
         (rparen (string-index-right proc-stat #\) ))
         (cmd (string-copy proc-stat (+ 1 lparen) rparen))
         (ppid (string->number (car (string-tokenize proc-stat char-set:graphic
                                                     (+ 4 rparen))))))
    (make-proc cmd pid ppid uid args)))

(define (get-pids dir)
  (filter number? (map string->number (directory dir))))

(define (read-procs)
  (reverse (map build-proc (get-pids "/proc"))))

(define get-username
  (let ((cache (make-hash-table)))
    (lambda (uid)
      (if (hash-table-exists? cache uid)
          (hash-table-ref cache uid)
          (let ((name (car (user-information uid))))
            (hash-table-set! cache uid name)
            name)))))

(define (show-tree start procs)
  (define (show-children depth more-at-depth parent last-uid)
    (let* ((children (filter (lambda (proc) (= (proc-ppid proc) parent)) procs))
           (children (sort children (lambda (a b) (string> (proc-cmd a) (proc-cmd b)))))
           (depth (+ 1 depth)))
      (if (null? children)
          '()
          (let ((last (car children))
                (rest (cdr children)))
            (for-each
             (show-proc depth #f (cons (list depth #t) more-at-depth) last-uid)
             (reverse rest))
            ((show-proc depth #t (cons (list depth #f) more-at-depth) last-uid)
             last)))))
  (define ((show-proc depth last more-at-depth last-uid) proc)
    (if (not (= depth 0))
        (do ((i 1 (+ i 1)))
            ((> i depth))
          (let ((more (cadr (assoc i more-at-depth))))
            (format #t "  ~a"
                    (if (= i depth)
                        (if last "`-" "|-")
                        (if more "| " "  "))))))
    (format #t "~a,~a" (proc-cmd proc) (proc-pid proc))
    (if (not (= (proc-uid proc) last-uid))
        (format #t ",~a" (get-username (proc-uid proc))))
    (if (> (length (proc-args proc)) 1)
        (format #t " ~a" (string-join (map escape (cdr (proc-args proc))) " ")))
    (format #t "~%")
    (show-children depth more-at-depth (proc-pid proc) (proc-uid proc)))
  (let ((root (find (lambda (proc) (= (proc-pid proc) start)) procs)))
    ((show-proc 0 #t '() 0) root)))

(define (main args)
  (show-tree 1 (read-procs)))
