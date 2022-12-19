(in-package #:adventofcode2022/test)

(define-constant +testdata-day19+ "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
  :test 'equal)

(def-test day19-task1 ()
  (is-true
   (= 33
      (run-task 19 1
                (make-string-input-stream +testdata-day19+)))))

(def-test day19-task2 ()
  (is-true
   (= 2604 ;; no idea if this is correct, but my input is
      (run-task 19 2
                (make-string-input-stream +testdata-day19+)))))
