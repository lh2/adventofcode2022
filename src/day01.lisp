(defpackage #:adventofcode2022/day01
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day01)

(defun task1 (inputs)
  (let ((max 0)
        (current 0))
    (dolist (input inputs)
      (if (null input)
          (progn
            (when (> current max)
              (setf max current))
            (setf current 0))
          (incf current input)))
    (when (> current max)
      (setf max current))
    max))

(defun task2 (inputs)
  (let ((elves nil)
        (current 0))
    (dolist (input inputs)
      (if (null input)
          (progn
            (push current elves)
            (setf current 0))
          (incf current input)))
    (push current elves)
    (setf elves (sort elves #'>))
    (apply #'+ (subseq elves 0 3))))

(define-day 1
    (:translate-input (lambda (line)
                        (if (= (length line) 0)
                            nil
                            (parse-integer line))))
  #'task1
  #'task2)
