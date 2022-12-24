(defpackage #:adventofcode2022/day24
  (:use #:cl #:adventofcode2022)
  (:import-from #:queues
                #:make-queue
                #:qpush
                #:qpop))
(in-package #:adventofcode2022/day24)

(defparameter *neighbor-deltas* (list '(-1 0)
                                      '(0 -1)
                                      '(1 0)
                                      '(0 1)))

(defun coord+ (a b)
  (list (+ (car a) (car b))
        (+ (cadr a) (cadr b))))

(defun analyze-inputs (inputs)
  (loop with blizzards = (make-hash-table :test 'equal)
        with min-pos = (list 1 1)
        with max-pos = (list 0 0)
        with start-pos = (list 1 0)
        with end-pos = (list 0 0)
        for row in inputs
        for y from 0
        when (= y 0)
          do (setf (car max-pos) (- (length row) 2)
                   (car end-pos) (- (length row) 2))

        do (loop for column across row
                 for x from 0
                 for blizzard-direction = (case column
                                            (#\^ :up)
                                            (#\< :left)
                                            (#\> :right)
                                            (#\v :down))
                 when blizzard-direction
                   do (setf (gethash (list x y) blizzards)
                            (append (gethash (list x y) blizzards)
                                    (list blizzard-direction))))
        finally (progn
                  (setf (cadr max-pos) (1- y)
                        (cadr end-pos) y)
                  (return (values
                           blizzards
                           min-pos
                           max-pos
                           start-pos
                           end-pos)))))

(defun get-next-blizzard-state (blizzards min-pos max-pos)
  (loop with next-blizzards = (make-hash-table :test 'equal)
        with width = (1+ (- (car max-pos) (car min-pos)))
        with height = (1+ (- (cadr max-pos) (cadr min-pos)))
        for pos being the hash-key of blizzards using (hash-value dirs)
        do (loop for dir in dirs
                 for next-pos = (copy-seq pos)
                 do (case dir
                      (:up (decf (cadr next-pos)))
                      (:left (decf (car next-pos)))
                      (:right (incf (car next-pos)))
                      (:down (incf (cadr next-pos))))
                 do (setf next-pos (list
                                    (+ (mod (- (car next-pos) (car min-pos))
                                            width)
                                       (car min-pos))
                                    (+ (mod (- (cadr next-pos) (cadr min-pos))
                                            height)
                                       (cadr min-pos))))
                 do (setf (gethash next-pos next-blizzards)
                          (append (gethash next-pos next-blizzards)
                                  (list dir))))
        finally (return next-blizzards)))

(defun print-map (min-pos max-pos current-pos blizzards)
  (loop for y from (cadr min-pos) to (cadr max-pos)
        do (loop for x from (car min-pos) to (car max-pos)
                 for pos = (list x y)
                 for bs = (gethash pos blizzards)
                 do (format t "~A" (cond
                                     ((equal pos current-pos) "E")
                                     ((> (length bs) 9) "*")
                                     ((> (length bs) 1) (length bs))
                                     (bs (case (car bs)
                                           (:up "^")
                                           (:left "<")
                                           (:right ">")
                                           (:down "v")))
                                     (t "."))))
        do (format t "~%"))
  (format t "~%"))

(defun test-blizzards (inputs rounds)
  (multiple-value-bind (blizzards min-pos max-pos start-pos end-pos)
      (analyze-inputs inputs)
    (declare (ignore start-pos end-pos))
    (loop with current-blizzards = blizzards
          initially (print-map min-pos max-pos (list 0 0) current-blizzards)
          repeat rounds
          do (setf current-blizzards (get-next-blizzard-state current-blizzards min-pos max-pos))
          do (print-map min-pos max-pos (list 0 0) current-blizzards))))

(defun task1 (inputs)
  (multiple-value-bind (blizzards min-pos max-pos start-pos end-pos)
      (analyze-inputs inputs)
    (loop named outer
          with visited = (make-hash-table :test 'equal)
          with blizzard-states = (make-hash-table)
          with queue = (make-queue :simple-queue)
          initially (qpush queue (list 0 start-pos))
                    (setf (gethash 0 blizzard-states) blizzards)
          for (minute current-pos) = (qpop queue)
          while current-pos
          for next-minute = (1+ minute)
          for current-blizzards = (gethash minute blizzard-states)
          for next-blizzards = (or (gethash next-minute blizzard-states)
                                   (setf (gethash next-minute blizzard-states)
                                         (get-next-blizzard-state current-blizzards min-pos max-pos)))
          do (loop for neighbor-delta in *neighbor-deltas*
                   for next-pos = (coord+ current-pos neighbor-delta)
                   when (equal next-pos end-pos)
                     do (return-from outer next-minute)
                   when (and (>= (car next-pos) (car min-pos))
                             (>= (cadr next-pos) (cadr min-pos))
                             (<= (car next-pos) (car max-pos))
                             (<= (cadr next-pos) (cadr max-pos))
                             (null (gethash next-pos next-blizzards))
                             (null (gethash (list next-pos next-minute) visited)))
                     do (qpush queue (list next-minute next-pos))
                     and do (setf (gethash (list next-pos next-minute) visited) t))
          when (not (gethash current-pos next-blizzards))
            do (qpush queue (list next-minute current-pos)))))

(define-day 24
    ()
  #'task1
  nil)
