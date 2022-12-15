(defpackage #:adventofcode2022/day15
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day15)

(defun make-min-or-max-coordinate (test)
  (let ((x)
        (y))
    (lambda (cmd &optional check-value)
      (cond
        ((eq cmd :set)
         (when (or (null x) (funcall test (car check-value) x))
           (setf x (car check-value)))
         (when (or (null y) (funcall test (cadr check-value) y))
           (setf y (cadr check-value))))
        ((eq cmd :get)
         (list x y))))))

(defun manhattan-distance (p-1 p-2)
  (+ (abs (- (car p-1) (car p-2)))
     (abs (- (cadr p-1) (cadr p-2)))))

(defun coord-x-y+ (coord value)
  (list (+ (car coord) value)
        (+ (cadr coord) value)))

(defun coord-x-y- (coord value)
  (list (- (car coord) value)
        (- (cadr coord) value)))

(defun get-sensor-distances (inputs)
  (let ((distances (make-hash-table :test 'equal))
        (min-pos (make-min-or-max-coordinate #'<))
        (max-pos (make-min-or-max-coordinate #'>)))
    (loop for input in inputs
          for sensor = (car input)
          for beacon = (cadr input)
          for distance = (manhattan-distance sensor beacon)
          do (funcall min-pos :set (coord-x-y- sensor distance))
          do (funcall max-pos :set (coord-x-y+ sensor distance))
          do (setf (gethash sensor distances) distance))
    (values distances (funcall min-pos :get) (funcall max-pos :get))))

(defun get-scan-ranges (sensors y)
  (loop for sensor being the hash-key of sensors
        for range = (gethash sensor sensors)
        when (and (>= y (- (cadr sensor) range))
                  (<= y (+ (cadr sensor) range)))
          collect (list (+ (- (car sensor) range)
                           (abs (- (cadr sensor) y)))
                        (- (+ (car sensor) range)
                           (abs (- (cadr sensor) y))))))

(defun task1 (inputs)
  (multiple-value-bind (distances min-pos max-pos)
      (get-sensor-distances inputs)
    (let* ((check-y 2000000)
           (check-y (if (< (cadr max-pos) check-y)
                        10 ;; Test case
                        check-y))
           (beacons (loop with ht = (make-hash-table :test 'equal)
                          for (sensor beacon) in inputs
                          do (setf (gethash beacon ht) t)
                          finally (return ht))))
      (loop with scan-ranges = (get-scan-ranges distances check-y)
            for x from (car min-pos) to (car max-pos)
            when (and (not (gethash (list x check-y) beacons))
                      (loop for range in scan-ranges
                            thereis (and (>= x (car range))
                                         (<= x (cadr range)))))
              sum 1))))

(defun task2 (inputs)
  (multiple-value-bind (distances min-pos max-pos)
      (get-sensor-distances inputs)
    (declare (ignore min-pos))
    (let* ((search-area-min 0)
           (search-area-max 4000000)
           (search-area-max (if (< (cadr max-pos) search-area-max)
                                20 ;; Test case
                                search-area-max)))
      (loop named outer
            for y from search-area-min to search-area-max
            for scan-ranges = (get-scan-ranges distances y)
            do (loop for range in scan-ranges
                     for left-edge = (1- (car range))
                     for right-edge = (1+ (cadr range))
                     do (loop for x in (list left-edge right-edge)
                              when (and (>= x search-area-min)
                                        (<= x search-area-max))
                                unless (loop for range in scan-ranges
                                             thereis (and (>= x (car range))
                                                          (<= x (cadr range))))
                                  do (return-from outer (+ (* x 4000000) y))))))))

(define-day 15
    (:translate-input (lambda (line)
                        (let* ((parts (str:split " " line))
                               (sx (nth 2 parts))
                               (sx (subseq sx 2 (1- (length sx))))
                               (sy (nth 3 parts))
                               (sy (subseq sy 2 (1- (length sy))))
                               (bx (nth 8 parts))
                               (bx (subseq bx 2 (1- (length bx))))
                               (by (subseq (nth 9 parts) 2)))
                          (list (list (parse-integer sx)
                                      (parse-integer sy))
                                (list (parse-integer bx)
                                      (parse-integer by))))))
  #'task1
  #'task2)
