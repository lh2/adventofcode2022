(defpackage #:adventofcode2022/day09
  (:use #:cl #:adventofcode2022)
  (:import-from #:alexandria #:clamp))
(in-package #:adventofcode2022/day09)

(defun move-head (head dir)
  (cond
    ((char= dir #\U) (decf (cadr head)))
    ((char= dir #\D) (incf (cadr head)))
    ((char= dir #\L) (decf (car head)))
    ((char= dir #\R) (incf (car head)))))

(defun head-tail-touching-p (head tail)
  (and
   (<= (abs (- (car head) (car tail))) 1)
   (<= (abs (- (cadr head) (cadr tail))) 1)))

(defun move-tail (tail head)
  (incf (car tail)
        (clamp (- (car head) (car tail)) -1 1))
  (incf (cadr tail)
        (clamp (- (cadr head) (cadr tail)) -1 1)))

(defun task1 (inputs)
  (loop with visited-positions = (make-hash-table :test 'equal)
        with pos-head = (list 0 0)
        with pos-tail = (list 0 0)
        initially (setf (gethash (copy-seq pos-tail) visited-positions) t)
        for (dir steps) in inputs
        do (loop for i from 1 to steps
                 do (move-head pos-head dir)
                 unless (head-tail-touching-p pos-head pos-tail)
                   do (move-tail pos-tail pos-head)
                   and do (setf (gethash (copy-seq pos-tail) visited-positions) t))
        finally (return (hash-table-count visited-positions))))

(defun task2 (inputs)
  (loop with visited-positions = (make-hash-table :test 'equal)
        with positions = (loop repeat 10 collect (list 0 0))
        with pos-head = (car positions)
        with pos-tail = (car (last positions))
        initially (setf (gethash (list 0 0) visited-positions) t)
        for (dir steps) in inputs
        do (loop for i from 1 to steps
                 do (move-head pos-head dir)
                 do (loop with previous = pos-head
                          for knot in (cdr positions)
                          for i from 1
                          unless (head-tail-touching-p previous knot)
                            do (move-tail knot previous)
                          do (setf previous knot))
                 do (setf (gethash (copy-seq pos-tail) visited-positions) t))
        finally (return (hash-table-count visited-positions))))

(define-day 9
    (:translate-input (lambda (line)
                        (list (aref line 0)
                              (parse-integer (subseq line 2)))))
  #'task1
  #'task2)
