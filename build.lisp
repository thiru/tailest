;;;; Builds executables

(ql:quickload :tailest)
(in-package :tailest)
(sb-ext:save-lisp-and-die "tailest" :toplevel #'run :executable t :compression t)
