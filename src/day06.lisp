(defpackage #:adventofcode2022/day06
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day06)

(defun first-unique-n-characters-after (sequence n)
  (loop with data = sequence
        for i from n to (length data)
        for marker = (subseq data (- i n) i)
        when (= n (length (remove-duplicates marker)))
          do (return i)))

(defun task1 (inputs)
  (first-unique-n-characters-after (car inputs) 4))

(defun task2 (inputs)
  (first-unique-n-characters-after (car inputs) 14))

(define-day 6
    ()
  #'task1
  #'task2)
