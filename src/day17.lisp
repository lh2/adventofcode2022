(defpackage #:adventofcode2022/day17
  (:use #:cl #:adventofcode2022)
  (:import-from #:alexandria
                #:define-constant))
(in-package #:adventofcode2022/day17)

(defclass rock ()
  ((shape :initarg :shape
          :accessor shape)))

(defmethod width ((r rock))
  (array-dimension (shape r) 1))

(defmethod height ((r rock))
  (array-dimension (shape r) 0))

(defparameter *rocks*
  (vector (make-instance 'rock
                         :shape #2A((1 1 1 1)))
          (make-instance 'rock
                         :shape #2A((0 1 0)
                                    (1 1 1)
                                    (0 1 0)))
          (make-instance 'rock
                         :shape #2A((0 0 1)
                                    (0 0 1)
                                    (1 1 1)))
          (make-instance 'rock
                         :shape #2A((1)
                                    (1)
                                    (1)
                                    (1)))
          (make-instance 'rock
                         :shape #2A((1 1)
                                    (1 1)))))

(defclass cave ()
  ((positions :initform (make-hash-table :test 'equal)
              :reader positions)
   (width :initarg :width
          :reader width)
   (tip-y :initform -1
          :accessor tip-y)))

(defmethod put-rock ((c cave) (r rock) x y)
  (loop for rock-y below (height r)
        do (loop for rock-x below (width r)
                 for cave-x = (+ rock-x x)
                 for cave-y = (- y rock-y)
                 do (when (= (aref (shape r) rock-y rock-x) 1)
                      (when (> cave-y (tip-y c))
                        (setf (tip-y c) cave-y))
                      (setf (gethash (list cave-x cave-y)
                                     (positions c))
                            t)))))

(defmethod rock-collides-p ((c cave) (r rock) x y)
  (when (or (< x 0)
            (> (+ x (width r)) (width c))
            (< (- y (1- (height r))) 0))
    (return-from rock-collides-p t))
  (loop named outer
        for rock-y below (height r)
        do (loop for rock-x below (width r)
                 for cave-x = (+ rock-x x)
                 for cave-y = (- y rock-y)
                 when (and (= (aref (shape r) rock-y rock-x) 1)
                           (gethash (list cave-x cave-y)
                                    (positions c)))
                   do (return-from outer t))))

(defmethod print-cave ((c cave))
  (loop for y from (tip-y c) downto 0
        do (loop initially (format t "|")
                 for x below (width c)
                 do (format t (if (gethash (list x y) (positions c))
                                  "#" "."))
                 finally (format t "|~%"))
        finally (loop initially (format t "+")
                      for x below (width c)
                      do (format t "-")
                      finally (format t "+~%"))))

(defun task1 (input)
  (loop with cave = (make-instance 'cave :width 7)
        with dirs = (car input)
        with round = 0
        for i below 2022
        for rock = (aref *rocks* (mod i (length *rocks*)))
        do (loop with rock-x = 2
                 with rock-y = (+ (tip-y cave) 3 (height rock))
                 for dir = (aref dirs (mod round (length dirs)))
                 do (incf round)
                 do (cond
                      ((char= dir #\<)
                       (unless (rock-collides-p cave rock
                                                (1- rock-x)
                                                rock-y)
                         (decf rock-x)))
                      ((char= dir #\>)
                       (unless (rock-collides-p cave rock
                                                (1+ rock-x)
                                                rock-y)
                         (incf rock-x))))
                 if (rock-collides-p cave rock rock-x (1- rock-y))
                   do (put-rock cave rock rock-x rock-y)
                   and return nil
                 else
                   do (decf rock-y))
           ;;do (print-cave cave)
        finally (return (1+ (tip-y cave)))))

(define-day 17
    ()
  #'task1
  nil)
