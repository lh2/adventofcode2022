(defpackage #:adventofcode2022/day07
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day07)

(defgeneric size (entry))

(defclass fs-entry ()
  ((name :initarg :name
         :initform (error "Please specify a name")
         :reader name)))

(defmethod dir-p ((entry fs-entry))
  nil)

(defclass fs-directory (fs-entry)
  ((parent :initarg :parent
           :reader parent)
   (children :accessor children
             :initform nil)
   (size-cache :accessor size-cache
               :initform nil)))

(defun make-fs-directory (name &optional parent)
  (make-instance 'fs-directory
                 :name name
                 :parent parent))

(defmethod dir-p ((dir fs-directory))
  t)

(defmethod size ((dir fs-directory))
  (if (not (null (size-cache dir)))
      (size-cache dir)
      (setf (size-cache dir)
            (loop for child in (children dir)
                  sum (size child)))))

(defmethod add-child ((dir fs-directory) (child fs-entry))
  (push child (children dir)))

(defmethod get-child ((dir fs-directory) child-name)
  (if (string= child-name "..")
      (parent dir)
      (loop for child in (children dir)
            when (string= (name child) child-name)
              return child)))

(defclass fs-file (fs-entry)
  ((size :initarg :size
         :accessor size)))

(defun make-fs-file (name size)
  (make-instance 'fs-file :name name :size size))

(defun build-fs-tree (inputs)
  (loop with root = (make-fs-directory "")
        with current-dir = root
        for line in inputs
        for parts = (str:split " " line)
        do (if (string= (car parts) "$")
               (when (string= (cadr parts) "cd")
                 (if (string= (caddr parts) "/")
                     (setf current-dir root)
                     (setf current-dir (get-child current-dir (caddr parts)))))
               (if (string= (car parts) "dir")
                   (add-child current-dir (make-fs-directory (cadr parts) current-dir))
                   (add-child current-dir (make-fs-file (cadr parts)
                                                        (parse-integer (car parts))))))
        finally (return root)))

(defun get-all-directories (dir)
  (loop with dirs = nil
        for child in (children dir)
        when (dir-p child)
          do (push child dirs)
          and do (setf dirs (concatenate 'list
                                         dirs
                                         (get-all-directories child)))
        finally (return dirs)))

(defun task1 (inputs)
  (let ((tree (build-fs-tree inputs)))
    (loop for dir in (get-all-directories tree)
          when (<= (size dir) 100000)
            sum (size dir))))

(define-day 7
    ()
  #'task1
  nil)
