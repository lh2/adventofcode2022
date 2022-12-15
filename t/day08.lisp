(in-package #:adventofcode2022/test)

(define-constant +testdata-day08+ "30373
25512
65332
33549
35390"
  :test 'equal)

(def-test day08-task1 ()
  (is-true
   (= 21
      (run-task 8 1
                (make-string-input-stream +testdata-day08+)))))

(def-test day08-task2 ()
  (is-true
   (= 8
      (run-task 8 2
                (make-string-input-stream +testdata-day08+)))))
