(in-package #:adventofcode2022/test)

(define-constant +testdata-day16+ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"
  :test 'equal)

(def-test day16-find-shortest-path ()
  (let ((adventofcode2022/day16::*distance-cache*
          (make-hash-table :test 'equal)))
    (multiple-value-bind (start unopened-valves)
        (adventofcode2022/day16::make-graph
         (mapcar #'adventofcode2022/day16::parse-line
                 (str:lines +testdata-day16+)))
      (loop for unopened-valve in unopened-valves
            for valve-id = (adventofcode2022/day16::vid unopened-valve)
            for path-length = (adventofcode2022/day16::find-shortest-path start unopened-valve)
            for expected = (case valve-id
                             (:BB 1)
                             (:CC 2)
                             (:DD 1)
                             (:EE 2)
                             (:HH 5)
                             (:JJ 2)
                             (otherwise 0))
            do (is-true (= path-length expected))))))

(def-test day16-task1 ()
  (is-true
   (= 1651
      (run-task 16 1
                (make-string-input-stream +testdata-day16+)))))

(def-test day16-task2 ()
  (is-true
   (= 1707
      (run-task 16 2
                (make-string-input-stream +testdata-day16+)))))
