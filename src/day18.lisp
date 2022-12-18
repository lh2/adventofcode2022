(defpackage #:adventofcode2022/day18
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day18)

(defparameter *sides* (list (list 1 0 0)
                            (list 0 1 0)
                            (list 0 0 1)
                            (list -1 0 0)
                            (list 0 -1 0)
                            (list 0 0 -1)))

(defun coord+ (a b)
  (loop for i-1 in a
        for i-2 in b
        collect (+ i-1 i-2)))

(defun task1 (inputs)
  (let ((map (make-hash-table :test 'equal)))
    (loop for input in inputs
          do (setf (gethash input map) t))
    (loop for coord being the hash-key of map
          sum (loop for side in *sides*
                    when (not (gethash (coord+ coord side) map))
                      sum 1))))

(define-day 18
    (:translate-input (lambda (line)
                        (mapcar #'parse-integer (uiop:split-string line :separator '(#\,)))))
  #'task1
  nil)
