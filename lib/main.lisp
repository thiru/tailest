(in-package :tailest)

(defvar app-version 0.2)
(defvar app-updated "Jul 10 2015")

(defparameter debug-mode nil)
(defparameter debug-args '("-a" "--help" "-n" "23"))

(defparameter num-lines-default 30)

(defun run ()
  "Run the app."
  (if debug-mode (format t "Command-line args: ~A~%" (command-line-arguments)))
  (let* ((cmd-args (if debug-mode debug-args (command-line-arguments)))
         (parsed-args (parse-cmd-args cmd-args num-lines-default))
         (target-dir (getcwd))
         (files (directory-files target-dir)))
    (if debug-mode (format t "Parsed args: ~A~%" parsed-args))
    (if (null files)
      (format t "No files found in ~a~%" target-dir)
      (let ((num-lines (getf parsed-args :num-lines)))
        (format t "~{~a~^~%~}~%"
                (get-last-n-lines
                  (get-latest-file files)
                  (if (> num-lines 0)
                    num-lines
                    num-lines-default)))))))

(defmacro pl=> (l k v)
  "Sets the value of a plist key/value pair."
  `(setf (getf ,l ,k) ,v))

(defun parse-cmd-args (cmd-args default-num-lines)
  "Parse the given command-line arguments."
  (let ((parsed-args `(:show-help nil
                       :num-lines ,default-num-lines)))
    (dolist (arg cmd-args)
      (cond ((arg=? arg "--help" "-h")
             (pl=> parsed-args :show-help t))

             ((arg=? arg "--num-files" "-n")
              (pl=> parsed-args :num-lines (get-num-lines cmd-args arg)))))
    parsed-args))

(defun arg=? (arg-val &rest arg-names)
  "Determine whether `arg-val` is `equalp` to any value in `arg-names`."
  (if (null arg-names) (error "No argument names provided."))
  (find arg-val arg-names :test #'equalp))

(defun get-num-lines (cmd-args arg-name)
  "Get the argument specifying number of lines."
  (let ((arg-idx (position arg-name cmd-args :test #'equalp)))
    (if (or (null arg-idx)
            (< arg-idx 0)
            (>= (+ 1 arg-idx) (length cmd-args)))
      (values 0 0)
      (parse-integer (nth (+ 1 arg-idx) cmd-args) :junk-allowed t))))

(defun get-latest-file (files)
  "Find the last modified file in `files`."
  (let ((sorted-files (stable-sort files #'> :key #'safe-file-write-date)))
    (car sorted-files)))

(defun get-last-n-lines (file n)
  "Get the last `n` lines in `file`."
  (if debug-mode (format t "Num lines: ~A~%" n))
  (let ((last-lines (make-array n :initial-element nil))
        (idx 0)
        (final-lines nil))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line do
              (setf (aref last-lines idx) line)
              (setf idx (inc-wrap idx n))))
    (setf idx (dec-wrap idx n))
    (dotimes (i n)
      (unless (null (aref last-lines idx))
        (push (aref last-lines idx) final-lines))
      (setf idx (dec-wrap idx n)))
    final-lines))

(defun inc-wrap (i length)
  "Increment `i` if it's less than `length`, otherwise return 0."
  (if (= i (- length 1))
    0
    (+ 1 i)))

(defun dec-wrap (i length)
  "Decrement `i` if it's greater than 0, otherwise return one less than
   `length`."
  (if (= i 0)
    (- length 1)
    (- i 1)))
