(defpackage #:adventofcode2022/day10
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day10)

(defun sprite-covers-pixel-p (pixel sprite-pos)
  (<= (abs (- sprite-pos pixel)) 1))

(defun run-cpu (inputs &optional video-stream)
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
                    sum (* j x)
                  do (format video-stream "~A"
                             (if (sprite-covers-pixel-p (mod (1- j) 40) x)
                                 #\#
                                 #\.))
                  when (= 0 (mod j 40))
                    do (format video-stream "~%"))
        do (setf i ni)
        when (string= op "addx")
          do (incf x value)))

(defun task1 (inputs)
  (run-cpu inputs))

(defun task2 (inputs)
  (with-output-to-string (s)
    (run-cpu inputs s)))

(define-day 10
    (:translate-input (lambda (line)
                        (let ((parts (uiop:split-string line :separator '(#\Space))))
                          (when (cadr parts)
                            (setf (cadr parts) (parse-integer (cadr parts))))
                          parts)))
  #'task1
  #'task2)
