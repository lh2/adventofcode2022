(in-package #:adventofcode2022/test)

(defconstant +testdata-day09+ "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def-test day09-task1 ()
  (is-true
   (= 13
      (run-task 9 1
                (make-string-input-stream +testdata-day09+)))))

