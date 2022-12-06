(defsystem "adventofcode2022"
  :description "My solutions to the advent of code 2022"
  :version "0.0.1"
  :author "Lukas Henkel <lh@entf.net>"
  :licence "AGPLv3"
  :serial t
  :pathname "src"
  :depends-on ("trivia" "str")
  :components ((:file "main")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04")
               (:file "day05")
               (:file "day06")))

(defsystem "adventofcode2022/test"
  :description "My solutions to the advent of code 2022"
  :version "0.0.1"
  :author "Lukas Henkel <lh@entf.net>"
  :licence "AGPLv3"
  :serial t
  :pathname "t"
  :depends-on ("adventofcode2022" "fiveam")
  :components ((:file "package")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04")
               (:file "day05")
               (:file "day06")))
