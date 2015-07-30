;;;; Tailest build configuration

(asdf:defsystem :tailest
  :version "1.0"
  :description "Shows the last *n* lines of the last modified file in a directory"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "EULA")
               (:file "package")
               (:file "main")))
