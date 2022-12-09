(in-package #:adventofcode2022/test)

(defconstant +testdata-day09-task1+ "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defconstant +testdata-day09-task2+ "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def-test day09-task1 ()
  (is-true
   (= 13
      (run-task 9 1
                (make-string-input-stream +testdata-day09-task1+)))))

(def-test day09-task2 ()
  (is-true
   (= 36
      (run-task 9 2
                (make-string-input-stream +testdata-day09-task2+)))))
