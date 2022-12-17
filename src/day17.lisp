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

(defun find-pattern (list)
  (loop for start from 0
        thereis (loop for length from 500 to (floor (/ (- (length list) start) 2))
                      when (loop for i below length
                                 for c-1 = (elt list (+ start i))
                                 for c-2 = (elt list (+ start i length))
                                 always (= c-1 c-2))
                        do (return-from find-pattern (values start length)))))

(defun play-tetris (dirs n-rocks)
  (loop with cave = (make-instance 'cave :width 7)
        with round = 0
        with history = nil
        for i below n-rocks
        for rock = (aref *rocks* (mod i (length *rocks*)))
        do (loop with rock-x = 2
                 with rock-y = (+ (tip-y cave) 3 (height rock))
                 with old-tip = (tip-y cave)
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
                   and do (push (- (tip-y cave) old-tip) history)
                   and return nil
                 else
                   do (decf rock-y))
           ;;do (print-cave cave)
        finally (return-from play-tetris
                  (values (1+ (tip-y cave))
                          (coerce (reverse history)
                                  'vector)))))

(defun task1 (input)
  (play-tetris (car input) 2022))

(defun task2 (input)
  (let ((simulate-n-rounds 10000)
        (simulation-target-rounds 1000000000000))
    (multiple-value-bind (height history)
        (play-tetris (car input) simulate-n-rounds)
      (declare (ignore height))
      (multiple-value-bind (start length)
          (find-pattern history)
        (let* ((initial (reduce #'+ (subseq history 0 start)))
               (cycle (subseq history start (+ start length)))
               (diff-per-cycle (reduce #'+ cycle)))
          (multiple-value-bind (n-cycles n-rest)
              (floor (- simulation-target-rounds start) length)
            (+ initial
               (* n-cycles diff-per-cycle)
               (reduce #'+ (subseq cycle 0 n-rest)))))))))

(define-day 17
    ()
  #'task1
  #'task2)
