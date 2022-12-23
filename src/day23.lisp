(defpackage #:adventofcode2022/day23
  (:use #:cl #:adventofcode2022)
  (:import-from #:alexandria
                #:rotate
                #:hash-table-keys))
(in-package #:adventofcode2022/day23)

(defparameter *directions-order* (list '((0 -1) (-1 -1) (1 -1))
                                       '((0 1) (-1 1) (1 1))
                                       '((-1 0) (-1 -1) (-1 1))
                                       '((1 0) (1 -1) (1 1))))

(defun make-map (inputs)
  (loop with map = (make-hash-table :test 'equal)
        for input in inputs
        for y from 0
        do (loop for char across input
                 for x from 0
                 when (char= char #\#)
                   do (setf (gethash (list x y) map) t))
        finally (return map)))

(defun calculate-score (map)
  (loop with (min-y max-y min-x max-x) = (loop for elf being the hash-key of map
                                               minimize (car elf) into min-x
                                               minimize (cadr elf) into min-y
                                               maximize (car elf) into max-x
                                               maximize (cadr elf) into max-y
                                               finally (return (list min-y max-y min-x max-x)))
        for y from min-y to max-y
        sum (loop for x from min-x to max-x
                  unless (gethash (list x y) map)
                    sum 1)))

(defun print-map (map)
  (loop with (max-y max-x) = (loop for elf being the hash-key of map
                                   maximize (car elf) into max-x
                                   maximize (cadr elf) into max-y
                                   finally (return (list max-y max-x)))
        for y to max-y
        do (loop for x to max-x
                 do (format t "~A"
                            (if (gethash (list x y) map) #\# #\.)))
        do (format t "~%"))
  (format t "~%"))

(defun coord+ (a b)
  (list (+ (car a) (car b))
        (+ (cadr a) (cadr b))))

(defun elf-can-move-p (map elf direction)
  (loop for delta in direction
        always (not (gethash (coord+ elf delta) map))))

(defun elf-has-neighbor (map elf)
  (loop for delta-set in *directions-order*
        thereis (loop for delta in delta-set
                      for neighbor = (coord+ elf delta)
                      thereis (gethash neighbor map))))

(defun remove-impossible-steps (steps)
  (remove-if (lambda (step)
               (> (count-if (lambda (other-step)
                              (equal (cadr step) (cadr other-step)))
                            steps)
                  1))
             steps))

(defun move-elves (map &optional max-rounds)
  (loop with current-order = (copy-seq *directions-order*)
        for round from 1
        while (or (null max-rounds)
                  (<= round max-rounds))
        for proposed-steps = (loop for elf in (remove-if (lambda (elf)
                                                           (not (elf-has-neighbor map elf)))
                                                         (hash-table-keys map))
                                   for valid-direction = (loop for direction in current-order
                                                               when (elf-can-move-p map elf direction)
                                                                 return (car direction)
                                                               finally (return nil))
                                   when valid-direction
                                     collect (list elf (coord+ elf valid-direction)))
        
        for valid-moves = (remove-impossible-steps proposed-steps)
        while valid-moves
        do (loop for (previous next) in valid-moves                 
                 do (remhash previous map)
                 do (setf (gethash next map) t))
        do (setf current-order (rotate current-order (1- (length current-order))))
           ;;do (print-map map)
        finally (return (values (calculate-score map) round))))

(defun task1 (inputs)
  (nth-value 0 (move-elves (make-map inputs) 10)))

(defun task2 (inputs)
  (nth-value 1 (move-elves (make-map inputs))))

(define-day 23
    ()
  #'task1
  #'task2)
