;;;; Builds the executable

(ql:quickload :tailest)
(setf tailest:help-text (tailest:get-help-text "README.md"))
#+sbcl (sb-ext:save-lisp-and-die #+linux "tailest" #+windows "tailest.exe"
                                 :compression t 
                                 :executable t
                                 :save-runtime-options t
                                 :toplevel #'tailest:main)
#+ccl (save-application #+linux "tailest" #+windows "tailest.exe"
                        :error-handler :quit
                        :prepend-kernel t 
                        :toplevel-function #'tailest:main)
#-(or sbcl ccl) (error "Sorry, only SBCL and CCL are currently supported.")
