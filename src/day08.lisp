(defpackage #:adventofcode2022/day08
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day08)

(defun look-at-trees (map
                      seen-trees
                      max-d-1
                      max-d-2
                      init-d-1
                      init-d-2
                      d-1-step-fun
                      d-2-step-fun
                      d-1-is-x-p)
  (loop for d-1 = init-d-1 then (funcall d-1-step-fun d-1)
        while (and (>= d-1 0) (< d-1 max-d-1))
        sum (loop with last-tree-height = -1
                  for d-2 = init-d-2 then (funcall d-2-step-fun d-2)
                  while (and (>= d-2 0) (< d-2 max-d-2))
                  for x = (if d-1-is-x-p d-1 d-2)
                  for y = (if d-1-is-x-p d-2 d-1)
                  for tree-height = (aref (aref map y) x)
                  when (> tree-height last-tree-height)
                    do (setf last-tree-height tree-height)
                    and unless (gethash (cons x y) seen-trees)
                          do (setf (gethash (cons x y) seen-trees) t)
                          and sum 1)))

(defun task1 (inputs)
  (let* ((height (length inputs))
         (width (length (car inputs)))
         (map (coerce inputs 'vector))
         (seen-trees (make-hash-table :test 'equal)))
    (+
     (look-at-trees map seen-trees height width 0 0 #'1+ #'1+ nil)
     (look-at-trees map seen-trees height width 0 (1- width) #'1+ #'1- nil)
     (look-at-trees map seen-trees width height 0 0 #'1+ #'1+ t)
     (look-at-trees map seen-trees width height 0 (1- height) #'1+ #'1- t))))

(defun get-viewing-distance (map max-tree-height static-d static-d-is-x-p init max step-fun)
  (loop with count-trees = 0
        for d = init then (funcall step-fun d)
        while (and (>= d 0) (< d max))
        for x = (if static-d-is-x-p static-d d)
        for y = (if static-d-is-x-p d static-d)
        for tree-height = (aref (aref map y) x)
        do (incf count-trees)
        if (>= tree-height max-tree-height)
          return count-trees
        finally (return count-trees)))

(defun calculate-tree-scenic-score (map width height x y)
  (let ((spot-height (aref (aref map y) x)))
    (*
     (get-viewing-distance map spot-height y nil (1+ x) width #'1+)
     (get-viewing-distance map spot-height y nil (1- x) width #'1-)
     (get-viewing-distance map spot-height x t (1+ y) height #'1+)
     (get-viewing-distance map spot-height x t (1- y) height #'1-))))

(defun task2 (inputs)
  (loop with height = (length inputs)
        with width = (length (car inputs))
        with map = (coerce inputs 'vector)
        for x from 0 below width
        maximizing (loop for y from 0 below height
                         maximizing (calculate-tree-scenic-score
                                     map
                                     width height
                                     x y))))

(define-day 8
    (:translate-input (lambda (input)
                        (map 'vector
                             (lambda (char)
                               (- (char-code char) 48))
                             input)))
  #'task1
  #'task2)
