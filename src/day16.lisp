(defpackage #:adventofcode2022/day16
  (:use #:cl #:adventofcode2022)
  (:import-from #:alexandria
                #:define-constant
                #:hash-table-values
                #:map-combinations)
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
   (bid :initarg :bid
        :reader bid)
   (rate :initarg :rate
         :accessor rate)
   (next-valves :accessor next-valves)))

(defun make-graph (inputs)
  (let ((valves (make-hash-table)))
    (loop for input in inputs
          for bit from 0
          do (setf (gethash (car input) valves)
                   (make-instance 'valve
                                  :vid (car input)
                                  :bid (ash 1 bit)
                                  :rate (cadr input))))
    (loop for input in inputs
          for valve = (gethash (car input) valves)
          for next-valves = (mapcar (lambda (id)
                                      (gethash id valves))
                                    (caddr input))
          do (setf (next-valves valve) next-valves))
    (values (gethash :AA valves)
            (loop with ht = (make-hash-table)
                  for valve being the hash-value of valves
                  do (setf (gethash (bid valve) ht) valve)
                  finally (return ht))
            (reduce #'logior
                    (mapcar #'bid
                            (remove-if (lambda (valve)
                                         (= (rate valve) 0))
                                       (hash-table-values valves)))))))

(defun get-active-bits (integer)
  (loop for i below (integer-length integer)
        when (logbitp i integer)
          collect (ash 1 i)))

(defparameter *distance-cache* nil)

(defun find-shortest-path (from to)
  (let* ((key (cons (bid from) (bid to)))
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
      (setf (gethash key *distance-cache*) shortest-path))))

(defparameter *pressure-cache* nil)

(defun calculate-max-pressure (valve-map current-valve unopened-valves remaining-minutes)
  (if (or (<= remaining-minutes 0)
          (= unopened-valves 0))
      0
      (loop for unopened-valve-bid in (get-active-bits unopened-valves)
            for unopened-valve = (gethash unopened-valve-bid valve-map)
            for path-length = (find-shortest-path current-valve unopened-valve)
            for remaining-minutes-new = (- remaining-minutes path-length 1)
            when (> remaining-minutes-new 0)
              maximize (+ (* (rate unopened-valve) remaining-minutes-new)
                          (get-max-pressure valve-map
                                            unopened-valve
                                            (logxor unopened-valves unopened-valve-bid)
                                            remaining-minutes-new)))))

(defun get-max-pressure (valve-map current-valve unopened-valves remaining-minutes)
  (let* ((key (list (bid current-valve)
                    unopened-valves
                    remaining-minutes))
         (cached (gethash key *pressure-cache*)))
    (when cached
      (return-from get-max-pressure cached))
    (let ((max-pressure (calculate-max-pressure valve-map
                                                current-valve
                                                unopened-valves
                                                remaining-minutes)))
      (when *pressure-cache*
        (setf (gethash key *pressure-cache*) max-pressure))
      max-pressure)))

(defun map-human-elephant-valves (func valves)
  (loop with valve-bids = (get-active-bits valves)
        for i from 0 to (length valve-bids)
        do (map-combinations
            (lambda (combination)
              (let ((elephant (remove-if
                               (lambda (bid)
                                 (member bid combination))
                               valve-bids)))
                (funcall func
                         (reduce #'logior combination)
                         (reduce #'logior elephant))))
            valve-bids
            :length i)))

(defun task1 (inputs)
  (let ((*distance-cache* (make-hash-table :test 'equal))
        (*pressure-cache* (make-hash-table :test 'equal)))
    (multiple-value-bind (start valve-map unopened-valves)
        (make-graph inputs)
      (get-max-pressure valve-map
                        start
                        unopened-valves
                        30))))

(defun task2 (inputs)
  (let ((*distance-cache* (make-hash-table :test 'equal))
        (*pressure-cache* (make-hash-table :test 'equal))
        (max-pressure 0))
    (multiple-value-bind (start valve-map unopened-valves)
        (make-graph inputs)
      (map-human-elephant-valves
       (lambda (human elephant)
         (setf max-pressure
               (max max-pressure
                    (+ (get-max-pressure valve-map
                                         start
                                         human
                                         26)
                       (get-max-pressure valve-map
                                         start
                                         elephant
                                         26)))))
       unopened-valves))
    max-pressure))

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
  #'task2)
