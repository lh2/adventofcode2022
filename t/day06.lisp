(in-package #:adventofcode2022/test)

(define-constant +testdata-day06+
    (list (list "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7 19)
          (list "bvwbjplbgvbhsrlpgdmjqwftvncz" 5 23)
          (list "nppdvjthqldpwncqszvftbrmjlhg" 6 23)
          (list "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10 29)
          (list "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11 26))
  :test 'equal)

(def-test day06-task1 ()
  (loop for (data m-1 m-2) in +testdata-day06+
        do (is-true
            (= m-1
               (run-task 6 1
                         (make-string-input-stream data))))))

(def-test day06-task2 ()
  (loop for (data m-1 m-2) in +testdata-day06+
        do (is-true
            (= m-2
               (run-task 6 2
                         (make-string-input-stream data))))))
