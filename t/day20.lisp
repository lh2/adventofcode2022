(in-package #:adventofcode2022/test)

(define-constant +testdata-day20+ "1
2
-3
3
-2
0
4"
  :test 'equal)

(def-test day20-task1 ()
  (is-true
   (= 3
      (run-task 20 1
                (make-string-input-stream +testdata-day20+)))))

(def-test day20-task2 ()
  (is-true
   (= 1623178306
      (run-task 20 2
                (make-string-input-stream +testdata-day20+)))))
