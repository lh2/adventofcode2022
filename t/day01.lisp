(in-package #:adventofcode2022/test)

(define-constant +testdata-day01+ "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"
  :test 'equal)

(def-test day01-task1 ()
  (is-true
   (= 24000
      (run-task 1 1
                (make-string-input-stream +testdata-day01+)))))

(def-test day01-task2 ()
  (is-true
   (= 45000
      (run-task 1 2
                (make-string-input-stream +testdata-day01+)))))
