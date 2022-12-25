(defpackage #:adventofcode2022/day25
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day25)

(defun parse-snafu (string)
  (loop with n = 0
        for digit across (reverse string)
        for m = 1 then (* m 5)
        do (incf n (* m (case digit
                          (#\2 2)
                          (#\1 1)
                          (#\- -1)
                          (#\= -2)
                          (otherwise 0))))
        finally (return n)))

(defun %format-snafu (number)
  (when (zerop number)
    (return-from %format-snafu nil))
  (let* ((m (mod number 5))
         (l (case m
              (3 #\=)
              (4 #\-)
              (otherwise (code-char (+ m 48)))))
         (r (cond
              ((or (= m 0))
               (%format-snafu (/ number 5)))
              ((or (= m 1) (= m 2))
               (%format-snafu (floor number 5)))
              ((or (= m 3) (= m 4))
               (%format-snafu (floor (+ number 2) 5))))))
    (cons l r)))

(defun format-snafu (number)
  (coerce (reverse (%format-snafu number)) 'string))

(defun task1 (inputs)
  (format-snafu
   (loop for input in inputs
         sum (parse-snafu input))))

(define-day 25
    ()
  #'task1
  nil)
