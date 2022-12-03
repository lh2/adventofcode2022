(defpackage #:adventofcode2022/day02
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day02)

(defun task1 (inputs)
  (let ((score 0))
    (dolist (input inputs)
      (if (= (car input) (cdr input))
          (incf score 3)
          (trivia:match input
            ((or (cons 1 2)
                 (cons 2 3)
                 (cons 3 1))
             (incf score 6))))
      (incf score (cdr input)))
    score))

(defun task2 (inputs)
  (let ((score 0))
    (dolist (input inputs)
      (cond
        ((= (cdr input) 2)
         (incf score 3))
        ((= (cdr input) 3)
         (incf score 6)))
      (if (= (cdr input) 2)
          (incf score (car input))
          (trivia:match input
            ((or (cons 1 1)
                 (cons 2 3))
             (incf score 3))
            ((or (cons 1 3)
                 (cons 3 1))
             (incf score 2))
            ((or (cons 2 1)
                 (cons 3 3))
             (incf score 1)))))
    score))

(define-day 2
    (:translate-input (lambda (line)
                        (cons (- (char-code (aref line 0)) 64)
                              (- (char-code (aref line 2)) 87))))
  #'task1
  #'task2)
