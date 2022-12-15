(in-package #:adventofcode2022/test)

(define-constant +testdata-day13+ "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"
  :test 'equal)

(def-test day13-compare-lists ()
  (is-true (adventofcode2022/day13::compare-lists
            #(1 1 3 1 1)
            #(1 1 5 1 1)))
  (is-true (not (adventofcode2022/day13::compare-lists
                 #(#(#() #(3 2 10)))
                 #(#(#() 2 #() 1 #(4 1)) #(3 #(#(8 4 0 7 8) 4 2))
                   #(#(9 #(2 1 8 2) #(6 0 3 1 1)) 4) #(10 2 2)))))
  (is-true (adventofcode2022/day13::compare-lists
            #(#() #(#(0 2 #(8) #(10 7) #(2 8 9 6)) 2 9 7) #(#(#()))
              #(10 4 0 #(8 #(10 7 0 3) #(6 5) #(4 1 2 4)))
              #(10 4 #(7 5 #(10 4) 5) #(#(6 4 0) 7) 7))
            #(#() #(7 #(#(7 6)))
              #(8 #(10 #(2 6) #(9 7 0 5 10))
                #(#(8)))
              #(#(#() 9 0 #(3 5 8) 9) 5)
              #(2 9)))))

(def-test day13-task1 ()
  (is-true
   (= 13
      (run-task 13 1
                (make-string-input-stream +testdata-day13+)))))

(def-test day13-task2 ()
  (is-true
   (= 140
      (run-task 13 2
                (make-string-input-stream +testdata-day13+)))))
