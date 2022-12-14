(in-package #:adventofcode2022/test)

(defconstant +testdata-day14+ "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def-test day14-task1 ()
  (is-true
   (= 24
      (run-task 14 1
                (make-string-input-stream +testdata-day14+)))))

