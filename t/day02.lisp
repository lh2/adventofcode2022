(in-package #:adventofcode2022/test)

(defconstant +testdata-day02+ "A Y
B X
C Z
")

(def-test day02-task1 ()
  (is-true
   (= 15
      (run-task 2 1
                (make-string-input-stream +testdata-day02+)))))
