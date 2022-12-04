(defpackage #:adventofcode2022/day04
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day04)

(defun range-contains-p (begin-1 end-1 begin-2 end-2)
  (or (and (>= begin-1 begin-2) (<= end-1 end-2))
      (and (>= begin-2 begin-1) (<= end-2 end-1))))

(defun range-intersects-p (begin-1 end-1 begin-2 end-2)
  (and (<= begin-1 end-2)
       (<= begin-2 end-1)))

(defun task1 (inputs)
  (loop for input in inputs
        when (range-contains-p
              (caar input)
              (cdar input)
              (cadr input)
              (cddr input))
          sum 1))

(defun task2 (inputs)
  (loop for input in inputs
        when (range-intersects-p
              (caar input)
              (cdar input)
              (cadr input)
              (cddr input))
          sum 1))

(define-day 4
    (:translate-input (lambda (line)
                        (let* ((pair (str:split "," line))
                               (range-1 (str:split "-" (car pair)))
                               (range-2 (str:split "-" (cadr pair))))
                          `((,(parse-integer (car range-1)) . ,(parse-integer (cadr range-1))) .
                            (,(parse-integer (car range-2)) . ,(parse-integer (cadr range-2)))))))
  #'task1
  #'task2)
