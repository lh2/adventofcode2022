(defpackage #:adventofcode2022/day14
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day14)

(defun make-map (inputs)
  (loop with map = (make-hash-table :test 'equal)
        with min-x = 500
        with max-x = 500
        with max-y = nil
        for input in inputs
        do (loop with last = (pop input)
                 for point in input
                 do (loop with axis = (if (= (car last) (car point)) #'cadr #'car)
                          with from = (funcall axis last)
                          with to = (funcall axis point)
                          with step-fun = (if (< from to) #'1+ #'1-)
                          for i = from then (funcall step-fun i)
                          for current-point = (if (eq axis #'car)
                                                  (list i (cadr last))
                                                  (list (car last) i))
                          when (or (null min-x) (< (car current-point) min-x))
                            do (setf min-x (car current-point))
                          when (or (null max-x) (> (car current-point) max-x))
                            do (setf max-x (car current-point))
                          when (or (null max-y) (> (cadr current-point) max-y))
                            do (setf max-y (cadr current-point))
                          do (setf (gethash current-point map) #\#)
                          while (not (= i to)))
                 do (setf last point))
        finally (return (values map (list min-x 0) (list max-x max-y)))))

(defun print-map (map min-point max-point)
  (loop for y from (cadr min-point) to (cadr max-point)
        do (loop for x from (car min-point) to (car max-point)
                 for cell = (gethash (list x y) map)
                 do (format t "~A" (if cell cell #\.)))
        do (format t "~%")))

(defun count-sand (map)
  (loop for cell being the hash-value of map
        when (char= cell #\o)
          sum 1))

(defun task1 (inputs)
  (multiple-value-bind (map min-point max-point)
      (make-map inputs)
    (loop with x = 500
          with y = 0
          while (and (>= x (car min-point))
                     (<= x (car max-point))
                     (<= y (cadr max-point)))
          for point = (list x y)
          for down = (list x (1+ y))
          for down-left = (list (1- x) (1+ y))
          for down-right = (list (1+ x) (1+ y))
          if (loop for next in (list down down-left down-right)
                   when (null (gethash next map))
                     do (setf x (car next))
                     and do (setf y (cadr next))
                     and do (return nil)
                   finally (return t))
            do (setf x 500)
            and do (setf y 0)
            and do (setf (gethash point map) #\o)
                   ;;and do (print-map map min-point max-point)
          )
    (count-sand map)))

(define-day 14
    (:translate-input (lambda (line)
                        (mapcar (lambda (x)
                                  (mapcar #'parse-integer
                                          (uiop:split-string x :separator '(#\,))))
                                (str:split " -> " line))))
  #'task1
  nil)
