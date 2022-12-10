(defpackage #:adventofcode2022/day10
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day10)

(defun task1 (inputs)
  (loop with x = 1
        with i = 1
        for (op value) in inputs
        for ni = i
        if (string= op "addx")
          do (incf ni 2)
        else
          do (incf ni 1)
        sum (loop for j from i below ni
                  when (or (= j 20)
                           (= j 60)
                           (= j 100)
                           (= j 140)
                           (= j 180)
                           (= j 220))
                    sum (* j x))
        do (setf i ni)
        when (string= op "addx")
          do (incf x value)))

(define-day 10
    (:translate-input (lambda (line)
                        (let ((parts (uiop:split-string line :separator '(#\Space))))
                          (when (cadr parts)
                            (setf (cadr parts) (parse-integer (cadr parts))))
                          parts)))
  #'task1
  nil)
