(defvar my-buffer)

(defun my-save (user line)
  (let ((filename (format "~/Documents/users/%s" user)))
    (with-temp-file filename
      (insert line))))


(defun my-line-by-line (buf)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
        (beginning-of-line)
        (let ((user (thing-at-point 'symbol t))
              (line (thing-at-point 'line t)))
          (message "yay %s %s" user line)
          (my-save user line))

        (forward-line 1)))))

(my-line-by-line my-buffer)
