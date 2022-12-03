(defpackage #:adventofcode2022/day03
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day03)

(defun calculate-priority (result)
  (if (char>= result #\a)
      (- (char-code result) 96)
      (- (char-code result) 38)))

(defun task1 (inputs)
  (loop for input in inputs
        sum (loop with compartment-size = (/ (length input) 2)
                  with compartment1 = (subseq input 0 compartment-size)
                  with compartment2 = (subseq input compartment-size)
                  with use-item2 = nil
                  for item1 across compartment1
                  for item2 across compartment2
                  until (or
                         (find item1 compartment2)
                         (and
                          (find item2 compartment1)
                          (setf use-item2 t)))
                  finally (return
                            (calculate-priority (if use-item2 item2 item1))))))

(defun task2 (inputs)
  (loop with head = inputs
        for end from 3
        when (= 0 (mod end 3))
          sum (loop for item = #\A then (cond ((char= item #\Z) #\a)
                                              ((char= item #\z) nil)
                                              (t (code-char (1+ (char-code item)))))
                    while item
                    until (loop for i from 0 to 2
                                always (find item (elt head i)))
                    finally (return (calculate-priority item)))
          and do (setf head (cdddr head))
        while head))

(define-day 3
    ()
  #'task1
  #'task2)
