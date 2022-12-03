(in-package #:adventofcode2022/test)

(defconstant +testdata-day03+ "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def-test day03-task1 ()
  (is-true
   (= 157
      (run-task 3 1
                (make-string-input-stream +testdata-day03+)))))

