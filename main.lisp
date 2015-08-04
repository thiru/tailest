(in-package :tailest)

(defparameter app-version 1.0)
(defparameter app-updated "Jul 24 2015")

(defparameter debug-mode nil)

(defparameter num-lines-default 30)
(defparameter help-text "Help was not loaded during build.")

(defun main (&optional argv)
  "Run the app."
  (if debug-mode (format t "Command-line args: ~A~%" argv))
  (let* ((parsed-args (parse-argv argv))
         (target-dir (getcwd))
         (files (directory-files target-dir)))
    (if debug-mode (format t "Parsed args: ~A~%" parsed-args))
    (cond ((getf parsed-args :invalid-arg)
           (format t "Invalid argument: '~A'~%"
                   (getf parsed-args :invalid-arg)))
          ((getf parsed-args :show-help)
           (format t help-text))
          ((getf parsed-args :show-version)
           (format t "~a~%" app-version))
          ((null files)
           (format t "No files found in ~a~%" target-dir))
          (t
           (show-last-n-lines files parsed-args num-lines-default)))))

(defun get-help-text (file-name)
  "Load the help content from the `file-name`. The first line of this content
   will be suffixed with the version and date last updated."
  (let ((first-line-read? nil)
        (help-text (make-string-output-stream)))
    (with-open-file (stream file-name :direction :input)
      (loop for line = (read-line stream nil)
            while line do
            (if first-line-read?
              (format help-text "~a~%" line)
              (format help-text "~a v~a (~a)~%" line app-version app-updated))
            (setf first-line-read? t)))
    (get-output-stream-string help-text)))

(defun show-last-n-lines (files parsed-args num-lines-default)
  "Show the last `n` lines of the last modified file in `files`."
  (let ((num-lines (getf parsed-args :num-lines)))
    (format t "~A~%"
            (get-last-n-lines
              (get-latest-file files)
              (if (or (null num-lines) (<= num-lines 0))
                num-lines-default
                num-lines)))))

(defmacro pl=> (l k v)
  "Sets the value of a plist key/value pair."
  `(setf (getf ,l ,k) ,v))

(defun parse-argv (argv)
  "Parse the given command-line arguments."
  (let ((parsed-args '())
        (skip-next-arg? nil))
    (format t "Initial parsed-args: ~A~%" parsed-args)
    (dolist (arg argv)
      (cond (skip-next-arg?
             (setf skip-next-arg? nil))
        
            ((arg=? arg "--debug" "-d")
             (setf debug-mode t))

            ((arg=? arg "--help" "-h")
             (pl=> parsed-args :show-help t))

            ((arg=? arg "--num-files" "-n")
             (pl=> parsed-args :num-lines (get-num-lines argv arg))
             (setf skip-next-arg? t))

            ((arg=? arg "--version" "-v")
             (pl=> parsed-args :show-version t))
            
            (t
             (pl=> parsed-args :invalid-arg arg))))
    parsed-args))

(defun arg=? (arg-val &rest arg-names)
  "Determine whether `arg-val` is `equalp` to any value in `arg-names`."
  (if (null arg-names) (error "No argument names provided."))
  (find arg-val arg-names :test #'equalp))

(defun get-num-lines (argv arg-name)
  "Get the argument specifying number of lines."
  (let ((arg-idx (position arg-name argv :test #'equalp)))
    (if (or (null arg-idx)
            (< arg-idx 0)
            (>= (+ 1 arg-idx) (length argv)))
      (values 0 0)
      (parse-integer (nth (+ 1 arg-idx) argv) :junk-allowed t))))

(defun get-latest-file (files)
  "Find the last modified file in `files`."
  (let ((sorted-files (stable-sort files #'> :key #'safe-file-write-date)))
    (car sorted-files)))

(defun get-last-n-lines (file n)
  "Get the last `n` lines in `file`."
  (if debug-mode (format t "Num lines: ~A~%" n))
  (with-open-file (stream file)
    (if debug-mode (format t "First char: ~A~%" (read-char-no-hang stream)))
    (if debug-mode (format t "File length: ~A~%" (file-length stream)))
    (let ((file-pos (- (file-length stream) 1))
          (char nil)
          (new-line-count 0))
      (file-position stream file-pos)
      (loop (when (or (zerop file-pos) (>= new-line-count n))
              (return))
            (decf file-pos)
            (file-position stream file-pos)
            (setf char (read-char-no-hang stream))
            (if (equalp char #\newline) (incf new-line-count))
            (if debug-mode (format t "Pos: ~A | Char: ~A~%" file-pos char)))
      (incf file-pos)
      (file-position stream file-pos)
      (if debug-mode (format t "Final file position: ~A~%" file-pos))
      (let ((final-string (make-string (- (file-length stream) (+ file-pos 2)))))
        (read-sequence final-string stream)
        final-string))))

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
