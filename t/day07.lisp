(in-package #:adventofcode2022/test)

(defconstant +testdata-day07+ "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def-test day07-task1 ()
  (is-true
   (= 95437
      (run-task 7 1
                (make-string-input-stream +testdata-day07+)))))

(def-test day07-task2 ()
  (is-true
   (= 24933642
      (run-task 7 2
                (make-string-input-stream +testdata-day07+)))))
