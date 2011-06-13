(defun readfile (filename &optional external-format)
    (if (null external-format) (setq external-format :latin-1))
    (with-open-file (f filename :external-format external-format)
        (let* ((len (file-length f))
               (data (make-string len)))
            (values data (read-sequence data f)))))

