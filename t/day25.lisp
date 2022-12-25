(in-package #:adventofcode2022/test)

(define-constant +testdata-day25+ "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"
  :test 'equal) 

(def-test day25-task1 ()
  (is-true
   (equal "2=-1=0"
          (run-task 25 1
                    (make-string-input-stream +testdata-day25+)))))
