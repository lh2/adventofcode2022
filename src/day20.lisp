(defpackage #:adventofcode2022/day20
  (:use #:cl #:adventofcode2022))
(in-package #:adventofcode2022/day20)

(defun mix (sequence)
  (loop with length = (1- (length sequence))
        for i from 0 to length
        for index = (position-if (lambda (item)
                                   (= (cadr item) i))
                                 sequence)
        for item-to-mix = (aref sequence index)
        for number-to-mix = (car item-to-mix)
        for new-index = (mod (+ index number-to-mix) length)
        do (cond
             ((> new-index index)
              (let ((after (subseq sequence (1+ index) (1+ new-index))))
                (replace sequence after :start1 index)))
             ((< new-index index)
              (let ((before (subseq sequence new-index index)))
                (replace sequence before :start1 (1+ new-index)))))
        do (setf (aref sequence new-index) item-to-mix)))

(defun convert-sequence (inputs &key map-number)
  (coerce (loop for number in inputs
                for i from 0
                collect (list (if map-number
                                  (funcall map-number number)
                                  number)
                              i))
          'vector))

(defun find-coordinates (sequence)
  (let ((length (length sequence))
        (zero (position-if (lambda (item)
                             (zerop (car item)))
                           sequence)))
    (+ (car (aref sequence (mod (+ zero 1000) length)))
       (car (aref sequence (mod (+ zero 2000) length)))
       (car (aref sequence (mod (+ zero 3000) length))))))

(defun task1 (inputs)
  (let ((sequence (convert-sequence inputs)))
    (mix sequence)
    (find-coordinates sequence)))

(defun task2 (inputs)
  (let ((sequence (convert-sequence inputs :map-number (lambda (number)
                                                         (* number 811589153)))))
    (loop repeat 10 do (mix sequence))
    (find-coordinates sequence)))

(define-day 20
    (:translate-input #'parse-integer)
  #'task1
  #'task2)
