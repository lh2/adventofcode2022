(defpackage #:adventofcode2022/day18
  (:use #:cl #:adventofcode2022)
  (:import-from #:queues
                #:make-queue
                #:qpush
                #:qpop
                #:qsize))
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

(defun coord-compare (a b test)
  (loop for i-1 in a
        for i-2 in b
        always (funcall test i-1 i-2)))

(defun task1 (inputs)
  (let ((map (make-hash-table :test 'equal)))
    (loop for input in inputs
          do (setf (gethash input map) t))
    (loop for coord being the hash-key of map
          sum (loop for side in *sides*
                    when (not (gethash (coord+ coord side) map))
                      sum 1))))

(defun task2 (inputs)
  (multiple-value-bind (map min-pos max-pos)
      (loop with map = (make-hash-table :test 'equal)
            for input in inputs
            do (setf (gethash input map) t)
            minimize (car input) into min-x
            minimize (cadr input) into min-y
            minimize (caddr input) into min-z
            maximize (car input) into max-x
            maximize (cadr input) into max-y
            maximize (caddr input) into max-z
            finally (return (values map
                                    (list min-x min-y min-z)
                                    (list max-x max-y max-z))))
    (setf min-pos (coord+ min-pos '(-1 -1 -1)))
    (setf max-pos (coord+ max-pos '(1 1 1)))
    (loop with queue = (make-queue :simple-queue)
          with visited = (make-hash-table :test 'equal)
          with surface-area = 0
          initially (qpush queue min-pos)
          while (> (qsize queue) 0)
          for coord = (qpop queue)
          do (loop for side in *sides*
                   for side-coord = (coord+ coord side)
                   do (if (gethash side-coord map)
                          (incf surface-area)
                          (when (and (not (gethash side-coord visited))
                                     (coord-compare side-coord min-pos #'>=)
                                     (coord-compare side-coord max-pos #'<=))
                            (qpush queue side-coord)
                            (setf (gethash side-coord visited) t))))
          finally (return surface-area))))

(define-day 18
    (:translate-input (lambda (line)
                        (mapcar #'parse-integer (uiop:split-string line :separator '(#\,)))))
  #'task1
  #'task2)
