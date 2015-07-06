(in-package :tailest)

(defvar app-version 0.1)
(defvar app-updated "Jul 4 2015")

(defun run ()
  (let* ((target-dir (getcwd))
         (files (directory-files target-dir)))
    (if (null files)
      (format t "No files found in ~a~%" target-dir)
      (format t "~{~a~^~%~}~%" (get-last-n-lines (get-latest-file files) 20)))))

(defun get-latest-file (files)
  (let ((sorted-files (stable-sort files #'> :key #'safe-file-write-date)))
    (car sorted-files)))

(defun get-last-n-lines (file n)
  (let ((last-lines (make-array n :initial-element nil))
        (idx 0)
        (final-lines nil))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line do
              ;(format t "~a <= ~a~%" idx line)
              (setf (aref last-lines idx) line)
              (setf idx (roll-forward idx n))))
    ;(format t "idx = ~a~%" idx)
    (setf idx (roll-backward idx n))
    (dotimes (i n)
      (unless (null (aref last-lines idx))
        (push (aref last-lines idx) final-lines))
      (setf idx (roll-backward idx n)))
    final-lines))

(defun roll-forward (i length)
  (if (= i (- length 1))
    0
    (+ 1 i)))

(defun roll-backward (i length)
  (if (= i 0)
    (- length 1)
    (- i 1)))
