(defpackage #:adventofcode2022/day11
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day11)

(defclass monkey ()
  ((items :initform nil
          :accessor items)
   (operation-fun :accessor operation-fun)
   (test-fun :accessor test-fun)
   (inspection-count :initform 0
                     :accessor inspection-count)))

(defun parse-monkeys (inputs)
  (loop with monkeys = nil
        with current-monkey = nil
        with test-divisible-by = nil
        with test-if-true = nil
        for input in inputs
        do (cond
             ((str:starts-with-p "Monkey " input)
              (setf current-monkey (make-instance 'monkey))
              (push current-monkey monkeys))
             ((str:starts-with-p "  Starting items:" input)
              (setf (items current-monkey)
                    (mapcar (lambda (x) (parse-integer x))
                            (str:split ", " (subseq input 18)))))
             ((str:starts-with-p "  Operation:" input)
              (setf (operation-fun current-monkey)
                    (eval (read-from-string
                           (format nil "(lambda (old) (~A old))" (subseq input 23))))))
             ((str:starts-with-p "  Test: divisible by" input)
              (setf test-divisible-by (subseq input 21)))
             ((str:starts-with-p "    If true: throw to monkey" input)
              (setf test-if-true (subseq input 29)))
             ((str:starts-with-p "    If false: throw to monkey" input)
              (setf (test-fun current-monkey)
                    (eval (read-from-string
                           (format nil
                                   "(lambda (item) (if (= (mod item ~A) 0) ~A ~A))"
                                   test-divisible-by
                                   test-if-true
                                   (subseq input 29)))))))
        finally (return (coerce (reverse monkeys) 'vector))))

(defun let-monkeys-play (monkeys rounds)
  (loop repeat rounds
        do (loop for monkey across monkeys
                 do (loop with operation-fun = (operation-fun monkey)
                          with test-fun = (test-fun monkey)
                          for item in (items monkey)
                          for new-item = (setf item
                                               (floor (/ (funcall operation-fun item) 3)))
                          for next-monkey-index = (funcall test-fun new-item)
                          for next-monkey = (aref monkeys next-monkey-index)
                          do (incf (inspection-count monkey))
                          do (setf (items next-monkey)
                                   (append (items next-monkey) (list new-item))))
                 do (setf (items monkey) nil))))

(defun task1 (inputs)
  (let ((monkeys (parse-monkeys inputs)))
    (let-monkeys-play monkeys 20)
    (apply #'*
           (map 'list
                (lambda (monkey)
                  (inspection-count monkey))
                (subseq (sort monkeys
                              (lambda (monkey-1 monkey-2)
                                (> (inspection-count monkey-1)
                                   (inspection-count monkey-2))))
                        0 2)))))

(define-day 11
    ()
  #'task1
  nil)
