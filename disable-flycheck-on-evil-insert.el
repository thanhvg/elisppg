
(defvar-local my-flycheck-flag nil
  "flag for insert mode")

(defun my-on-enter-insert()
  "check if `flycheck-mode' is on and turn it off"
  (when flycheck-mode
    (setq my-flycheck-flag t)
    (flycheck-mode -1)))

(defun my-on-exit-insert()
  "check if `flycheck-mode' is on. If so turn it off"
  (when my-flycheck-flag
    (flycheck-mode 1)))

(add-hook 'evil-insert-state-entry-hook #'my-on-enter-insert)
(add-hook 'evil-insert-state-exit-hook #'my-on-exit-insert)
