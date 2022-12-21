(in-package #:adventofcode2022/test)

(define-constant +testdata-day21+ "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"
  :test 'equal)

(def-test day21-task1 ()
  (is-true
   (= 152
      (run-task 21 1
                (make-string-input-stream +testdata-day21+)))))

(def-test day21-task2 ()
  (is-true
   (= 301
      (run-task 21 2
                (make-string-input-stream +testdata-day21+)))))
