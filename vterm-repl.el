(defmacro vterm-repl-with-repl (body)

  )

(defun vterm-repl--get-project-shell-buffer ()
  (concat "*"
          (spacemacs//current-layout-name)
          "-"
          (if (file-remote-p default-directory)
              "remote-"
            "")
          "vterm"
          (format "-%s" shell-pop-last-shell-buffer-index)
          "*"))

(defun vterm-repl-send-string (str)
  (with-current-buffer (vterm-repl--get-project-shell-buffer)
    (vterm-send-string str)))

(defun vterm-repl-send-buffer ()
  (interactive)
  (vterm-repl-send-string (buffer-substring-no-properties (point-min) (point-max))))

(defun vterm-repl-send-line ()
  (interactive)
  (vterm-repl-send-string
   (thing-at-point 'line t)))

(defun vterm-repl-send-function ()
  (interactive)
  (vterm-repl-send-string
   (thing-at-point 'defun t)))

(defun vterm-repl-send-region (start end)
  (interactive "r")
  (vterm-repl-send-string
   (buffer-substring-no-properties start end)))


;; (thing-at-point 'defun t)
