;;;; Builds the executable under SBCL

(ql:quickload :tailest)
(setf tailest:help-text (tailest:get-help-text "README.md"))
#+sbcl (sb-ext:save-lisp-and-die #+linux "tailest" #+windows "tailest.exe"
                                 :compression t 
                                 :executable t
                                 :save-runtime-options t
                                 :toplevel #'tailest:main)
#+ccl (save-application #+linux "tailest" #+windows "tailest.exe"
                          :toplevel-function #'tailest:main
                          :error-handler :quit
                          :prepend-kernel t)
#-(or sbcl ccl) (error "Unsupported Common Lisp platform.")
