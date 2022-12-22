(defpackage #:adventofcode2022/day22
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day22)

(defun turn (facing direction)
  (mod
   (+ facing
      (case direction
        (:left -1)
        (:right 1)
        (otherwise 0)))
   4))

(defun walk (coords facing)
  (let ((x (car coords))
        (y (cadr coords)))
    (case facing
      (0 (list (1+ x) y))
      (1 (list x (1+ y)))
      (2 (list (1- x) y))
      (3 (list x (1- y))))))

(defun parse-input (inputs)
  (loop with map = (make-hash-table :test 'equal)
        with last-line? = nil
        with min-x = nil
        for line in inputs
        for length = (length line)
        for y from 0
        when last-line?
          return (values
                  map
                  (loop with start = 0
                        with results = nil
                        for i from 0
                        while (< i length)
                        for char = (aref line i)
                        if (not (digit-char-p char))
                          do (push (parse-integer (subseq line start i)) results)
                          and do (push (case char (#\R :right) (#\L :left)) results)
                          and do (setf start (1+ i))
                        finally (push (parse-integer (subseq line start i)) results)
                        finally (return (nreverse results)))
                  (list min-x 0))
        when (= length 0)
          do (setf last-line? t)
        do (loop for x from 0 below length
                 for char = (aref line x)
                 for type = (case (aref line x)
                              (#\# :wall)
                              (#\. :empty)
                              (otherwise nil))
                 when (and (= y 0)
                           (eq type :empty)
                           (or (null min-x)
                               (< x min-x)))
                   do (setf min-x x)
                 when type
                   do (setf (gethash (list x y) map) type))))

(defun wrap-around (map pos last-pos)
  (let ((x (car pos))
        (y (cadr pos)))
    (if (= y (cadr last-pos))
        (list
         (if (> x (car last-pos))
             (loop for i from 0
                   for tile = (gethash (list i y) map)
                   when tile
                     return i)
             (loop for i from (car last-pos)
                   for (tile exists) = (multiple-value-list (gethash (list i y) map))
                   while exists
                   maximize i))
         y)
        (list
         x
         (if (> y (cadr last-pos))
             (loop for i from 0
                   for tile = (gethash (list x i) map)
                   when tile
                     return i)
             (loop for i from (1+ y)
                   for tile = (gethash (list x i) map)
                   while tile
                   maximize i))))))

(defun task1 (inputs)
  (multiple-value-bind (map directions start-pos)
      (parse-input inputs)
    (loop with facing = 0
          with current-pos = start-pos
          for direction in directions
          if (numberp direction)
            do (loop repeat direction
                     for next-coord = (walk current-pos facing)
                     for next-tile = (gethash next-coord map)
                     when (null next-tile)
                       do (setf next-coord (wrap-around map next-coord current-pos))
                       and do (setf next-tile (gethash next-coord map))
                     never (eq next-tile :wall)
                     do (setf current-pos next-coord))
          else
            do (setf facing (turn facing direction))
          finally (return (+ (* 1000 (1+ (cadr current-pos)))
                             (* 4 (1+ (car current-pos)))
                             facing)))))

(define-day 22
    ()
  #'task1
  nil)
