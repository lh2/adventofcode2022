(defpackage #:adventofcode2022/day05
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day05)

(defun parse-stack (stack-lines)
  (let* ((indices (mapcar (lambda (x)
                            (parse-integer x))
                          (str:split " " (pop stack-lines) :omit-nulls t)))
         (length (apply #'max indices))
         (stacks (make-array length :initial-element nil)))
    (loop for line in stack-lines
          do (loop for i from 0 below length
                   for crate-index = (1+ (* i 4))
                   for crate-id = (aref line crate-index)
                   when (not (char= #\Space crate-id))
                     do (push crate-id
                              (aref stacks i))))
    stacks))

(defun parse-input (inputs)
  (loop with stack-lines = nil
        with rules = nil
        with rules-p = nil
        for input in inputs
        if (= 0 (length input))
          do (setf rules-p t)
        else if rules-p
               do (let ((parts (str:split " " input)))
                    (push (list (parse-integer (second parts))
                                (parse-integer (fourth parts))
                                (parse-integer (sixth parts)))
                          rules))
        else
          do (push input stack-lines)
        finally (return (values (parse-stack stack-lines)
                                (reverse rules)))))

(defun task1 (inputs)
  (multiple-value-bind (stacks rules)
      (parse-input inputs)
    (loop for rule in rules
          do (loop for i from 1 to (car rule)
                   for from = (1- (cadr rule))
                   for to = (1- (caddr rule))
                   for crate = (pop (aref stacks from))
                   do (push crate (aref stacks to))))
    (coerce (loop for i from 0 below (length stacks)
                  collect (first (aref stacks i)))
            'string)))

(defmacro popn (place n)
  `(loop for i from 0 below ,n
         collect (pop ,place)))

(defun task2 (inputs)
  (multiple-value-bind (stacks rules)
      (parse-input inputs)
    (loop for rule in rules
          for from = (1- (cadr rule))
          for to = (1- (caddr rule))
          for crates = (popn (aref stacks from) (car rule))
          do (setf (aref stacks to)
                   (nconc crates (aref stacks to))))
    (coerce (loop for i from 0 below (length stacks)
                  collect (first (aref stacks i)))
            'string)))

(define-day 5
    ()
  #'task1
  #'task2)
