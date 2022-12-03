(defpackage #:adventofcode2022/day03
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day03)

(defun task1 (inputs)
  (loop for input in inputs
        sum (let ((compartment1 (car input))
                  (compartment2 (cdr input))
                  (result))
              (loop for item1 across compartment1
                    for item2 across compartment2
                    when (find item1 compartment2 :test #'equal)
                      do (progn
                           (setf result item1)
                           (loop-finish))
                    when (find item2 compartment1 :test #'equal)
                      do (progn
                           (setf result item2)
                           (loop-finish)))
              (if (char>= result #\a)
                  (- (char-code result) 96)
                  (- (char-code result) 38)))))

(define-day 3
    (:translate-input (lambda (line)
                        (let ((compartment-size (/ (length line) 2)))
                          (cons (subseq line 0 compartment-size)
                                (subseq line compartment-size)))))
  #'task1
  nil)
