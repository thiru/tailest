;;;; Builds the executable under Clozure Common Lisp

(ql:quickload :tailest)
(save-application "tailest.exe"
                  :toplevel-function #'tailest:run
                  :error-handler :quit
                  :prepend-kernel t)
