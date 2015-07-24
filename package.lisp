;;;; Tailest package definitions

(defpackage :tailest
  (:use :cl :asdf :uiop)
  (:documentation "Tailest core domain/API and console interface")
  (:export :app-version
           :app-updated
           :get-help-text
           :help-text
           :main))
