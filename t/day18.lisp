(in-package #:adventofcode2022/test)

(define-constant +testdata-day18+ "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"
  :test 'equal)

(def-test day18-task1 ()
  (is-true
   (= 64
      (run-task 18 1
                (make-string-input-stream +testdata-day18+)))))

(def-test day18-task2 ()
  (is-true
   (= 58
      (run-task 18 2
                (make-string-input-stream +testdata-day18+)))))
