(defpackage #:adventofcode2022
  (:use #:cl)
  (:export
   #:run-task
   #:add-task
   #:define-day))
(in-package #:adventofcode2022)

(defparameter *days* (make-hash-table))

(defun add-task (day task func)
  (let ((current (gethash day *days*)))
    (when (null current)
      (setf current (make-array 2)))
    (setf (aref current (1- task)) func)
    (setf (gethash day *days*) current)))

(defun run-task (day task &optional input-stream)
  (let* ((tasks (gethash day *days*))
         (taskfun (aref tasks (1- task)))
         (s (if (null input-stream)
                (open
                 (merge-pathnames
                  (make-pathname
                   :directory '(:relative "input")
                   :name (format nil "day~2,'0d" day)
                   :type "txt")
                  (asdf:system-source-directory "adventofcode2022")))
                input-stream)))
    (unwind-protect
         (funcall taskfun s)
      (close s))))

(defun parse-input (input-stream opts)
  (let ((translate-fun (getf opts :translate-input))
        (lines (uiop:slurp-stream-lines input-stream)))
    (when (not (null translate-fun))
      (setf lines (mapcar translate-fun lines)))
    lines))

(eval-when (:compile-toplevel)
  (defun make-add-task-form (day task opts form)
    `(add-task ,day ,task
               (lambda (input-stream)
                 (let ((inputs (parse-input input-stream (list ,@opts))))
                   (funcall ,form inputs))))))

(defmacro define-day (day opts task1 task2)
  `(progn
     ,(make-add-task-form day 1 opts task1)
     ,(when (not (null task2))
        (make-add-task-form day 2 opts task2))))
