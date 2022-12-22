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

(defun wrap-around (map pos last-pos dir)
  (declare (ignore dir))
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

(defparameter *wrap-map* (make-hash-table :test 'equal))

(loop for i below 50
      do (flet ((m (x y dir nx ny ndir)
                  (setf (gethash (list x y dir) *wrap-map*) (list nx ny ndir))))
           (m 50 i 2 0 (- 149 i) 0)
           (m (+ 50 i) 0 3 0 (+ 150 i) 0)

           (m (+ 100 i) 0 3 i 199 3)
           (m 149 i 0 99 (- 149 i) 2)
           (m (+ 100 i) 49 1 99 (+ 50 i) 2)

           (m 50 (+ 50 i) 2 i 100 1)
           (m 99 (+ 50 i) 0 (+ 100 i) 49 3)

           (m 99 (+ 100 i) 0 149 (- 49 i) 2)
           (m (+ 50 i) 149 1 49 (+ 150 i) 2)

           (m 0 (+ 100 i) 2 50 (- 49 i) 0)
           (m i 100 3 50 (+ 50 i) 0)

           (m 0 (+ 150 i) 2 (+ 50 i) 0 1)
           (m 49 (+ 150 i) 0 (+ 50 i) 149 3)
           (m i 199 1 (+ 100 i) 0 1)))

(defun wrap-cube (map pos last-pos dir)
  (declare (ignore map pos))
  (or (gethash (append last-pos (list dir))
               *wrap-map*)
      (error "Wrap result is null!")))

(defun walk-map (map directions start-pos wrap-function)
  (loop with facing = 0
        with current-pos = start-pos
        for direction in directions
        if (numberp direction)
          do (loop repeat direction
                   for next-coord = (walk current-pos facing)
                   for next-facing = facing
                   for next-tile = (gethash next-coord map)
                   when (null next-tile)
                     do (let ((wrap (funcall wrap-function map next-coord current-pos facing)))
                          (setf next-coord (subseq wrap 0 2))
                          (when (> (length wrap) 2)
                            (setf next-facing (caddr wrap))))
                     and do (setf next-tile (gethash next-coord map))
                   never (eq next-tile :wall)
                   do (setf current-pos next-coord
                            facing next-facing))
        else
          do (setf facing (turn facing direction))
        finally (return (progn
                          (list (* 4 (1+ (car current-pos))) (* 1000 (1+ (cadr current-pos))) facing)
                          (+ (* 1000 (1+ (cadr current-pos)))
                             (* 4 (1+ (car current-pos)))
                             facing)))))

(defun task1 (inputs)
  (multiple-value-bind (map directions start-pos)
      (parse-input inputs)
    (walk-map map directions start-pos #'wrap-around)))

(defun task2 (inputs)
  (multiple-value-bind (map directions start-pos)
      (parse-input inputs)
    (walk-map map directions start-pos #'wrap-cube)))

(define-day 22
    ()
  #'task1
  #'task2)
