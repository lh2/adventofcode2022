(in-package #:adventofcode2022/test)

(define-constant +testdata-day24+ "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"
  :test 'equal)

(define-constant +testdata-day24-simple+ "#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#"
  :test 'equal)

(def-test day24-task1 ()
  (is-true
   (= 18
      (run-task 24 1
                (make-string-input-stream +testdata-day24+)))))
