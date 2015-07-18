;;;; Builds the executable under SBCL

(ql:quickload :tailest)
(setf tailest:help-text (tailest:get-help-text "README.md"))
(sb-ext:save-lisp-and-die "tailest"
                          :compression t 
                          :executable t
                          :save-runtime-options t
                          :toplevel #'tailest:main)
