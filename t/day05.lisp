(in-package #:adventofcode2022/test)

(define-constant +testdata-day05+ "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"
  :test 'equal)

(def-test day05-task1 ()
  (is-true
   (string= "CMZ"
            (run-task 5 1
                      (make-string-input-stream +testdata-day05+)))))

(def-test day05-task2 ()
  (is-true
   (string= "MCD"
            (run-task 5 2
                      (make-string-input-stream +testdata-day05+)))))

