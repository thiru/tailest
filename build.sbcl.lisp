;;;; Builds the executable under SBCL

(ql:quickload :tailest)
(in-package :tailest)
(setf help-text (get-help-text "README.md"))
(sb-ext:save-lisp-and-die "tailest"
                          :toplevel #'run
                          :executable t
                          :compression t)
