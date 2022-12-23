(in-package #:adventofcode2022/test)

(define-constant +testdata-day23+ "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."
  :test 'equal)

(def-test day23-task1 ()
  (is-true
   (= 110
      (run-task 23 1
                (make-string-input-stream +testdata-day23+)))))

(def-test day23-task2 ()
  (is-true
   (= 20
      (run-task 23 2
                (make-string-input-stream +testdata-day23+)))))
