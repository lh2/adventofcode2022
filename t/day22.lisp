(in-package #:adventofcode2022/test)

(define-constant +testdata-day22+ "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
  :test 'equal)

(def-test day22-task1 ()
  (is-true
   (= 6032
      (run-task 22 1
                (make-string-input-stream +testdata-day22+)))))
