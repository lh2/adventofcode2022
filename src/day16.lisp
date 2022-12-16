(defpackage #:adventofcode2022/day16
  (:use #:cl #:adventofcode2022)
  (:import-from #:alexandria
                #:define-constant
                #:hash-table-values)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:import-from #:queues
                #:make-queue
                #:qpush
                #:qpop))
(in-package #:adventofcode2022/day16)

(define-constant +input-line-regex+
  "Valve ([^ ]+) .* rate=(\\d+); .* valves? (.*)"
  :test 'equal)

(defclass valve ()
  ((vid :initarg :vid
        :reader vid)
   (rate :initarg :rate
         :accessor rate)
   (next-valves :accessor next-valves)))

(defun make-graph (inputs)
  (let ((valves (make-hash-table)))
    (loop for input in inputs
          do (setf (gethash (car input) valves)
                   (make-instance 'valve
                                  :vid (car input)
                                  :rate (cadr input))))
    (loop for input in inputs
          for valve = (gethash (car input) valves)
          for next-valves = (mapcar (lambda (id)
                                      (gethash id valves))
                                    (caddr input))
          do (setf (next-valves valve) next-valves))
    (values (gethash :AA valves)
            (remove-if (lambda (valve)
                         (= (rate valve) 0))
                       (hash-table-values valves)))))

(defparameter *distance-cache* nil)

(defun find-shortest-path (from to)
  (let* ((key (cons (vid from) (vid to)))
         (cached (gethash key *distance-cache*)))
    (unless (null cached)
      (return-from find-shortest-path cached))
    (let ((shortest-path
            (loop named outer
                  with queue = (make-queue :simple-queue)
                  initially (qpush queue (list 0 from))
                  while t
                  for (length node) = (qpop queue)
                  do (loop for next-node in (next-valves node)
                           when (eq to next-node)
                             do (return-from outer (1+ length))
                           do (qpush queue (list (1+ length) next-node))))))
      (setf (gethash key *distance-cache*) shortest-path)
      shortest-path)))

(defun get-highest-pressure (current-valve unopened-valves remaining-minutes)
  (if (or (<= remaining-minutes 0)
          (= (length unopened-valves) 0))
      0
      (loop for unopened-valve in unopened-valves
            for path-length = (find-shortest-path current-valve unopened-valve)
            for remaining-minutes-new = (- remaining-minutes path-length 1)
            when (> remaining-minutes-new 0)
              maximize (+ (* (rate unopened-valve) remaining-minutes-new)
                          (get-highest-pressure unopened-valve
                                                (remove unopened-valve unopened-valves)
                                                remaining-minutes-new)))))

(defun task1 (inputs)
  (let ((*distance-cache* (make-hash-table :test 'equal)))
    (multiple-value-bind (start unopened-valves)
        (make-graph inputs)
      (get-highest-pressure start unopened-valves 30))))

(defun parse-line (input)
  (register-groups-bind (valve rate next-valves)
      (+input-line-regex+ input)
    (list (intern valve :keyword)
          (parse-integer rate)
          (mapcar (lambda (valve)
                    (intern valve :keyword))
                  (str:split ", " next-valves)))))

(define-day 16
    (:translate-input #'parse-line)
  #'task1
  nil)
