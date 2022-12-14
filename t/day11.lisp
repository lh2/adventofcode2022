(in-package #:adventofcode2022/test)

(define-constant +testdata-day11+ "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"
  :test 'equal)

(def-test day11-task1 ()
  (is-true
   (= 10605
      (run-task 11 1
                (make-string-input-stream +testdata-day11+)))))

(def-test day11-task2 ()
  (is-true
   (= 2713310158
      (run-task 11 2
                (make-string-input-stream +testdata-day11+)))))
