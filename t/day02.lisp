(in-package #:adventofcode2022/test)

(define-constant +testdata-day02+ "A Y
B X
C Z
"
  :test 'equal)

(def-test day02-task1 ()
  (is-true
   (= 15
      (run-task 2 1
                (make-string-input-stream +testdata-day02+)))))

(def-test day02-task2 ()
  (is-true
   (= 12
      (run-task 2 2
                (make-string-input-stream +testdata-day02+)))))
