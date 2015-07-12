;;;; Builds the executable under Clozure Common Lisp

(ql:quickload :tailest)
(in-package :tailest)
(setf help-text (get-help-text "README.md"))
(save-application "tailest.exe"
                  :toplevel-function #'run
                  :error-handler :quit
                  :prepend-kernel t)
