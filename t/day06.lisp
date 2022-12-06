(in-package #:adventofcode2022/test)

(defparameter *testdata-day06*
  (list (list "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7)
        (list "bvwbjplbgvbhsrlpgdmjqwftvncz" 5)
        (list "nppdvjthqldpwncqszvftbrmjlhg" 6)
        (list "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10)
        (list "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11)))

(def-test day06-task1 ()
  (loop for (data marker) in *testdata-day06*
        do (is-true
            (= marker
               (run-task 6 1
                         (make-string-input-stream data))))))
