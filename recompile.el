(defun me-recompile (dir)
  (seq-do
   (lambda (fname)
     (when (file-exists-p fname)
       (delete-file fname)))
   (directory-files-recursively dir "\\.elc$" t))
  (byte-recompile-directory dir 0 arg))
