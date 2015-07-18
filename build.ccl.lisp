;;;; Builds the executable under Clozure Common Lisp

(ql:quickload :tailest)
(setf tailest:help-text (tailest:get-help-text "README.md"))
(save-application "tailest.exe"
                  :toplevel-function #'tailest:main
                  :error-handler :quit
                  :prepend-kernel t)
