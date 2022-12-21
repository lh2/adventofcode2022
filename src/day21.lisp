(defpackage #:adventofcode2022/day21
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day21)

(defparameter *yell-operations* (make-hash-table))
(setf (gethash :+ *yell-operations*) #'+)
(setf (gethash :- *yell-operations*) #'-)
(setf (gethash :* *yell-operations*) #'*)
(setf (gethash :/ *yell-operations*) #'/)

(defparameter *yell-operations-reverse* (make-hash-table))
(setf (gethash :+ *yell-operations-reverse*) #'-)
(setf (gethash :- *yell-operations-reverse*) #'+)
(setf (gethash :* *yell-operations-reverse*) #'/)
(setf (gethash :/ *yell-operations-reverse*) #'*)

(defun monkey-yell (monkey monkeys)
  (if (listp monkey)
      (destructuring-bind (op monkey-1 monkey-2)
          monkey
        (let ((dep-1 (monkey-yell (gethash monkey-1 monkeys) monkeys))
              (dep-2 (monkey-yell (gethash monkey-2 monkeys) monkeys))
              (op-fun (gethash op *yell-operations*)))
          (funcall op-fun dep-1 dep-2)))
      monkey))

(defun monkey-yell-reverse (monkey monkeys monkey-deps)
  "This only works for the example, no idea why, maybe I'll debug this some other day."
  (let* ((dep-monkey-name (gethash monkey monkey-deps))
         (dep-monkey-rule (gethash dep-monkey-name monkeys))
         (dep-monkey-op (gethash (car dep-monkey-rule) *yell-operations-reverse*))
         (dep-monkey-deps (cdr dep-monkey-rule))
         (dep-monkey-other-monkey-car? (= 0 (position monkey dep-monkey-deps)))
         (dep-monkey-other-monkey (car (remove monkey dep-monkey-deps))))
    (cond
      ((eq dep-monkey-name :root)
       (monkey-yell (gethash dep-monkey-other-monkey monkeys) monkeys))
      (t (let ((args (list
                      (monkey-yell-reverse dep-monkey-name monkeys monkey-deps)
                      (monkey-yell (gethash dep-monkey-other-monkey monkeys) monkeys))))
           (when dep-monkey-other-monkey-car?
             (setf args (reverse args)))
           (apply dep-monkey-op args))))))

(defun task1 (inputs)
  (let ((monkeys (make-hash-table)))
    (loop for (name monkey) in inputs
          do (setf (gethash name monkeys) monkey))
    (monkey-yell (gethash :root monkeys) monkeys)))

#|(defun task2 (inputs)
(let ((monkeys (make-hash-table))
(monkey-deps (make-hash-table)))
(loop for (name monkey) in inputs
do (setf (gethash name monkeys) monkey)
when (consp monkey)
do (loop for dep in (cdr monkey)
do (setf (gethash dep monkey-deps) name)))
(monkey-yell-reverse :humn monkeys monkey-deps)))|#

(defun task2 (inputs)
  (let ((monkeys (make-hash-table)))
    (loop for (name monkey) in inputs
          do (setf (gethash name monkeys) monkey))
    (let ((root (gethash :root monkeys)))
      (setf (car root) :-)
      (loop for inc = 1000000000 then (floor inc 10)
            while (> inc 0)
            thereis (loop for res = (monkey-yell root monkeys)
                          thereis (= res 0)
                          when (< res 0)
                            do (setf (gethash :humn monkeys) (- (gethash :humn monkeys) inc))
                            and return nil
                          while (> res 0)
                          do (setf (gethash :humn monkeys) (+ (gethash :humn monkeys) inc))))
      (gethash :humn monkeys))))

(define-day 21
    (:translate-input (lambda (line)
                        (destructuring-bind (name rule)
                            (str:split ": " line)
                          (list (intern (str:upcase name) :keyword)
                                (if (str:containsp " " rule)
                                    (destructuring-bind (monkey-1 op monkey-2)
                                        (str:split " " rule)
                                      (list (intern op :keyword)
                                            (intern (str:upcase monkey-1) :keyword)
                                            (intern (str:upcase monkey-2) :keyword)))
                                    (parse-integer rule))))))
  #'task1
  #'task2)
