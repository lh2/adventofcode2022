(defpackage #:adventofcode2022/day02
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day02)

(defun task1 (inputs)
  (let ((score 0))
    (dolist (input inputs)
      (if (= (car input) (cdr input))
          (incf score 3)
          (trivia:match input
            ((cons 1 2)
             (incf score 6))
            ((cons 2 3)
             (incf score 6))
            ((cons 3 1)
             (incf score 6))))
      (incf score (cdr input)))
    score))

(define-day 2
    (:translate-input (lambda (line)
                        (cons (- (char-code (aref line 0)) 64)
                              (- (char-code (aref line 2)) 87))))
  #'task1
  nil)
