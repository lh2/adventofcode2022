(defpackage #:adventofcode2022/day21
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day21)

(defun cache-monkey (monkey)
  (let ((cache))
    (lambda (monkeys)
      (if cache
          cache
          (setf cache (funcall monkey monkeys))))))

(defun task1 (inputs)
  (let ((monkeys (make-hash-table :test 'equal)))
    (loop for (name monkey) in inputs
          do (setf (gethash name monkeys) (cache-monkey monkey)))
    (funcall (gethash "root" monkeys) monkeys)))

(define-day 21
    (:translate-input (lambda (line)
                        (destructuring-bind (name rule)
                            (str:split ": " line)
                          (list name
                                (eval `(lambda (monkeys)
                                         ,@(if (str:containsp " " rule)
                                               (destructuring-bind (monkey-1 op monkey-2)
                                                   (str:split " " rule)
                                                 (list `(,(intern op)
                                                         (funcall (gethash ,monkey-1 monkeys) monkeys)
                                                         (funcall (gethash ,monkey-2 monkeys) monkeys))))
                                               (list
                                                '(declare (ignore monkeys))
                                                (parse-integer rule)))))))))
  #'task1
  nil)
