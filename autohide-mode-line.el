(defun my-turn-on-mode-line-maybe ()
  (when hidden-mode-line-mode
    (hidden-mode-line-mode -1)))

(defun my-turn-on-mode-line ()
  (run-with-idle-timer 1 nil #'my-turn-on-mode-line-maybe))

(defun my-pre-command-hook-handle ()
  ;; (message "tahnh %s" this-command)
  (when (and (or (eq this-command 'evil-next-line)
                 (eq this-command 'evil-previous-line))
             (not hidden-mode-line-mode))
    (hidden-mode-line-mode)
    (my-turn-on-mode-line)))

(add-hook 'pre-command-hook #'my-pre-command-hook-handle)

;; (defun my-mode-line ()
;;   (hidden-mode-line-mode -1))

;; (add-hook 'focus-out-hook  #'my-mode-line)
