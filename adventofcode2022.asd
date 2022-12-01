(defsystem "adventofcode2022"
  :description "My solutions to the advent of code 2022"
  :version "0.0.1"
  :author "Lukas Henkel <lh@entf.net>"
  :licence "AGPLv3"
  :serial t
  :pathname "src"
  :components ((:file "main")
               (:file "day01")))

(defsystem "adventofcode2022/test"
  :description "My solutions to the advent of code 2022"
  :version "0.0.1"
  :author "Lukas Henkel <lh@entf.net>"
  :licence "AGPLv3"
  :serial t
  :pathname "t"
  :depends-on ("adventofcode2022" "fiveam")
  :components ((:file "package")
               (:file "day01")))
