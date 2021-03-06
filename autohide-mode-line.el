;;; -*- lexical-binding: t -*-
(defun my-turn-on-mode-line-maybe (buff)
  (when (buffer-live-p buff)
   (with-current-buffer buff
     (when hidden-mode-line-mode
       (hidden-mode-line-mode -1)))))

;; less than 1 sec, will fliker screen on eldoc trigger
(defun my-turn-on-mode-line (buff)
  ;; (run-with-idle-timer 1 nil #'my-turn-on-mode-line-maybe buff))
  (run-with-idle-timer 1 nil  (lambda () (my-turn-on-mode-line-maybe buff))))

(defun my-pre-command-hook-handle ()
  ;; (message "tahnh %s" this-command)
  (when (and (or (eq this-command 'evil-next-line)
                 (eq this-command 'evil-previous-line))
             (not hidden-mode-line-mode))
    ;; (message "thanh on %s" (current-buffer))
    (hidden-mode-line-mode)
    (my-turn-on-mode-line (current-buffer))))

(add-hook 'pre-command-hook #'my-pre-command-hook-handle)
;; (add-hook 'pre-command-hook #'my-pre-command-hook-handle nil t)
;; (remove-hook 'pre-command-hook #'my-pre-command-hook-handle)

;; so when buffer is killed it should trigger this run on
;; next select buffer to make sure mode-line is back
;; without this hook, K on a symbol then q, mode-line is gone forever
;; (add-hook 'kill-buffer-hook #'my-turn-on-mode-line)
;; (remove-hook 'kill-buffer-hook #'my-turn-on-mode-line)

;; (defun my-mode-line ()
;;   (hidden-mode-line-mode -1))
;; (remove-hook 'focus-in-hook  #'my-mode-line)
;; (remove-hook 'focus-out-hook  #'my-mode-line)
