(defpackage #:adventofcode2022/day06
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day06)

(defun task1 (inputs)
  (loop with data = (car inputs)
        for i from 4 to (length data)
        for marker = (subseq data (- i 4) i)
        when (= 4 (length (remove-duplicates marker)))
          do (return i)))

(define-day 6
    ()
  #'task1
  nil)
