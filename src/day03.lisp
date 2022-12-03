(defpackage #:adventofcode2022/day03
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day03)

(defun calculate-priority (result)
  (if (char>= result #\a)
      (- (char-code result) 96)
      (- (char-code result) 38)))

(defun task1 (inputs)
  (loop for input in inputs
        sum (let* ((compartment-size (/ (length input) 2))
                   (compartment1 (subseq input 0 compartment-size))
                   (compartment2 (subseq input compartment-size))
                   (result))
              (loop for item1 across compartment1
                    for item2 across compartment2
                    when (find item1 compartment2)
                      do (progn
                           (setf result item1)
                           (loop-finish))
                    when (find item2 compartment1)
                      do (progn
                           (setf result item2)
                           (loop-finish)))
              (calculate-priority result))))

(defun all-chars-from-to (from to)
  (loop for i from (char-code from) to (char-code to)
        collect (code-char i)))

(defun task2 (inputs)
  (let* ((items (concatenate 'list
                             (all-chars-from-to #\A #\Z)
                             (all-chars-from-to #\a #\z)))
         (group)
         (groups (loop for i from 1
                       for elf in inputs
                       do (push elf group)
                       when (= (mod i 3) 0)
                         collect group and do (setf group nil))))
    (loop for group in groups
          sum (let ((result))
                (loop for item in items
                      when (= 0 (length (remove-if (lambda (elf)
                                                     (find item elf))
                                                   group)))
                        do (progn
                             (setf result item)
                             (loop-finish)))
                (calculate-priority result)))))

(define-day 3
    ()
  #'task1
  #'task2)
