(defpackage #:adventofcode2022/day12
  (:use #:cl #:adventofcode2022)
  (:import-from #:queues
                #:qpush
                #:qpop
                #:qsize
                #:make-queue))
(in-package #:adventofcode2022/day12)

(defun get-passable-neighbors (map pos)
  (loop with width = (array-dimension map 0)
        with height = (array-dimension map 1)
        with pos-height = (aref map (first pos) (second pos))
        for dir in '((-1 0)
                     (1 0)
                     (0 -1)
                     (0 1))
        for x = (+ (first pos) (first dir))
        for y = (+ (second pos) (second dir))
        unless (or (< x 0) (< y 0)
                   (>= x width) (>= y height)
                   (< (1+ pos-height) (aref map x y)))
          collect (list x y)))

(defun find-shortest-route (map start-pos end-pos)
  (loop with queue = (make-queue :simple-queue)
        with steps = (make-hash-table :test 'equal)
        initially (setf (gethash start-pos steps) 0)
                  (qpush queue start-pos)
        while (> (qsize queue) 0)
        for pos = (qpop queue)
        for current-steps = (gethash pos steps)
        when (equal pos end-pos)
          return current-steps
        do (loop for next in (get-passable-neighbors map pos)
                 unless (gethash next steps)
                   do (setf (gethash next steps) (1+ current-steps))
                   and do (qpush queue next))))

(defun find-position (map char)
  (loop named outer
        for y from 0 below (length map)
        for row = (aref map y)
        do (loop for x from 0 below (length row)
                 when (char= (aref row x) char)
                   do (return-from outer (list x y)))))

(defun convert-map (map)
  (loop with height = (length map)
        with width = (length (aref map 0))
        with new-map = (make-array (list width height))
        for y from 0 below height
        for row = (aref map y)
        do (loop for x from 0 below width
                 for cell = (aref row x)
                 do (setf (aref new-map x y)
                          (cond
                            ((char= cell #\S) 0)
                            ((char= cell #\E) 25)
                            (t (- (char-code cell) 97)))))
        finally (return new-map)))

(defun task1 (inputs)
  (let* ((map (coerce inputs 'vector))
         (start-pos (find-position map #\S))
         (end-pos (find-position map #\E))
         (map (convert-map map)))
    (find-shortest-route map start-pos end-pos)))

(defun task2 (inputs)
  (loop with map = (coerce inputs 'vector)
        with end-pos = (find-position map #\E)
        with shortest-distance = nil
        with current-distance = nil
        initially (setf map (convert-map map))
        for x from 0 below (array-dimension map 0)
        do (loop for y from 0 below (array-dimension map 1)
                 when (= 0 (aref map x y))
                   do (setf current-distance
                            (find-shortest-route map (list x y) end-pos))
                   and when (or (null shortest-distance)
                                (and (not (null current-distance))
                                     (< current-distance shortest-distance)))
                         do (setf shortest-distance current-distance))
        finally (return shortest-distance)))

(define-day 12
    ()
  #'task1
  #'task2)
