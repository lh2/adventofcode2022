(in-package #:adventofcode2022/test)

(defconstant +testdata-day04+ "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(def-test day04-task1 ()
  (is-true
   (= 2
      (run-task 4 1
                (make-string-input-stream +testdata-day04+)))))
