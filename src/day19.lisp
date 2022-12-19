(defpackage #:adventofcode2022/day19
  (:use #:cl #:adventofcode2022)
  (:import-from #:cl-ppcre
                #:register-groups-bind))
(in-package #:adventofcode2022/day19)

(defun get-resource-index (resource)
  (cond
    ((eq resource :ore) 0)
    ((eq resource :clay) 1)
    ((eq resource :obsidian) 2)
    ((eq resource :geode) 3)
    (t (error "Unknown resource type ~S" resource))))

(defun getres (resource &optional field)
  (if field
      (aref field (get-resource-index resource))
      (lambda (x) (getres resource x))))

(defun (setf getres) (value resource field)
  (setf (elt field (get-resource-index resource)) value))

(defun %res-op (op resources-1 resources-2)
  (let* ((length (length resources-1))
         (new (make-array length)))
    (loop for i from 0 below length
          do (setf (aref new i)
                   (funcall op
                            (aref resources-1 i)
                            (aref resources-2 i))))
    new))

(declaim (inline res+))
(defun res+ (resources-1 resources-2)
  (%res-op #'+ resources-1 resources-2))

(declaim (inline res-))
(defun res- (resources-1 resources-2)
  (%res-op #'- resources-1 resources-2))

(defun can-build-robot-p (blueprint resources)
  (loop with depends = blueprint
        for i from 0 below (length depends)
        always (>= (aref resources i) (aref depends i))))

(defun build-robot (type blueprints robots resources)
  (let ((robots-delta (vector 0 0 0 0)))
    (incf (getres type robots-delta))
    (values
     (res+ robots robots-delta)
     (res- resources blueprints))))

(defparameter *max-geodes-cache* nil)
(defparameter *max-geode-by-minute-cache* nil)

(defun compute-cache-key (remaining-minutes robots resources)
  "This is a lot faster than puttin lists in 'equal tables."
  (logior
   (getres :ore resources)
   (ash (getres :clay resources) 16)
   (ash (getres :obsidian resources) 32)
   (ash (getres :geode resources) 48)
   (ash (getres :ore robots) 64)
   (ash (getres :clay robots) 80)
   (ash (getres :obsidian robots) 96)
   (ash (getres :geode robots) 112)
   (ash remaining-minutes 128)))

(defun max-geodes-for-blueprint (blueprint max-bots &optional (remaining-minutes 24) (robots (vector 1 0 0 0)) (resources (vector 0 0 0 0)))
  (when (<= remaining-minutes 0)
    (return-from max-geodes-for-blueprint (getres :geode resources)))
  (let* ((key (compute-cache-key remaining-minutes robots resources))
         (cached (gethash key *max-geodes-cache*))
         (current-geodes (getres :geode resources)))
    (when cached
      (return-from max-geodes-for-blueprint cached))
    (when (< current-geodes (or (gethash remaining-minutes *max-geode-by-minute-cache*) 0))
      (return-from max-geodes-for-blueprint 0))
    (setf (gethash remaining-minutes *max-geode-by-minute-cache*) current-geodes)
    (let ((produced (copy-seq robots))
          (results)
          (best-result))
      (when (loop for type in '(:geode :obsidian :clay :ore)
                  for robot-bp = (getres type blueprint)
                  when (and (< (getres type robots) (getres type max-bots))
                            (can-build-robot-p robot-bp resources))
                    do (multiple-value-bind (new-robots new-resources)
                           (build-robot type robot-bp robots resources)
                         (push (max-geodes-for-blueprint blueprint
                                                         max-bots
                                                         (1- remaining-minutes)
                                                         new-robots
                                                         (res+ new-resources produced))
                               results)
                         (when (eq type :geode)
                           (return nil)))
                  finally (return t))
        (push (max-geodes-for-blueprint blueprint
                                        max-bots
                                        (1- remaining-minutes)
                                        robots
                                        (res+ resources produced))
              results))
      (setf best-result (apply #'max results))
      (setf (gethash key *max-geodes-cache*) best-result))))

(defun get-max-robots (blueprint)
  (vector (apply #'max (map 'list (getres :ore) blueprint))
          (apply #'max (map 'list (getres :clay) blueprint))
          (apply #'max (map 'list (getres :obsidian) blueprint))
          9999))

(defun task1 (inputs)
  (loop for input in inputs
        for blueprint = (cadr input)
        for max-robots = (get-max-robots blueprint)
        sum (let ((*max-geodes-cache* (make-hash-table))
                  (*max-geode-by-minute-cache* (make-hash-table)))
              (let ((max-geodes (max-geodes-for-blueprint blueprint max-robots)))
                (* (car input) max-geodes)))))

(defun task2 (inputs)
  (apply #'*
         (loop for input in (subseq inputs 0 (min 3 (length inputs)))
               for blueprint = (cadr input)
               for max-robots = (get-max-robots blueprint)
               collect (let ((*max-geodes-cache* (make-hash-table))
                             (*max-geode-by-minute-cache* (make-hash-table)))
                         (let ((max-geodes (max-geodes-for-blueprint blueprint max-robots 32)))
                           max-geodes)))))

(define-day 19
    (:translate-input (lambda (input)
                        (let* ((label-rules (str:split ":" input :limit 2))
                               (id (parse-integer (cadr (str:split " " (car label-rules)))))
                               (rules (str:split "." (cadr label-rules) :omit-nulls t)))
                          (list id
                                (loop with robots = (vector
                                                     (vector 0 0 0 0)
                                                     (vector 0 0 0 0)
                                                     (vector 0 0 0 0)
                                                     (vector 0 0 0 0))
                                      for rule in rules
                                      do (register-groups-bind (type-str rest)
                                             ("Each (.*?) robot costs (.*)" rule)
                                           (let* ((type (intern (str:upcase type-str) :keyword))
                                                  (robot (getres type robots)))
                                             (loop for cost-str in (str:split "and" rest)
                                                   do (register-groups-bind (cost material)
                                                          ("(\\d+) (.*)" cost-str)
                                                        (let ((cost-int (parse-integer cost))
                                                              (dependency (intern
                                                                           (str:upcase (str:trim material))
                                                                           :keyword)))
                                                          (setf (getres dependency robot) cost-int))))))
                                      finally (return robots))))))
  #'task1
  #'task2)
