(in-package #:adventofcode2022/test)

(define-constant +testdata-day12+ "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
  :test 'equal)

(def-test day12-task1 ()
  (is-true
   (= 31
      (run-task 12 1
                (make-string-input-stream +testdata-day12+)))))

(def-test day12-task2 ()
  (is-true
   (= 29
      (run-task 12 2
                (make-string-input-stream +testdata-day12+)))))
