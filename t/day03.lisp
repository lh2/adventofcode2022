(in-package #:adventofcode2022/test)

(define-constant +testdata-day03+ "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
  :test 'equal)

(def-test day03-task1 ()
  (is-true
   (= 157
      (run-task 3 1
                (make-string-input-stream +testdata-day03+)))))

(def-test day03-task2 ()
  (is-true
   (= 70
      (run-task 3 2
                (make-string-input-stream +testdata-day03+)))))
