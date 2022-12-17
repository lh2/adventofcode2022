(in-package #:adventofcode2022/test)

(define-constant +testdata-day17+ ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  :test 'equal)

(def-test day17-task1 ()
  (is-true
   (= 3068
      (run-task 17 1
                (make-string-input-stream +testdata-day17+)))))

(def-test day17-task2 ()
  (is-true
   (= 1514285714288
      (run-task 17 2
                (make-string-input-stream +testdata-day17+)))))
