;;;; Tailest package definitions

(defpackage :tailest
  (:use :cl :glu :asdf :uiop)
  (:documentation "Tailest core domain/API and console interface")
  (:export
    :app-version
    :app-updated
    :run))
