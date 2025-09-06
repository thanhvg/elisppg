;; -*- lexical-binding: t; -*-

(require 'winum)

(defun winum-other-window-prefix (num)
  "Display the buffer of the next command in a new window NUM."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (let ((alist (append '((inhibit-same-window . t)) alist))
           window type)
       (setq window (window--display-buffer buffer (winum-get-window-by-number num) 'reuse alist))
       (cons window 'reuse)))
   nil "[other-window]")
  (message "Display next command buffer in a window num %s ..." num))


(defun winum-other-window-prefix-1 ()
  (interactive)
  (winum-other-window-prefix 1))

(defun winum-other-window-prefix-2 ()
  (interactive)
  (winum-other-window-prefix 2))

(defun winum-other-window-prefix-3 ()
  (interactive)
  (winum-other-window-prefix 3))

(defun winum-other-window-prefix-4 ()
  (interactive)
  (winum-other-window-prefix 4))

(defun winum-other-window-prefix-5 ()
  (interactive)
  (winum-other-window-prefix 5))

(defun winum-other-window-prefix-6 ()
  (interactive)
  (winum-other-window-prefix 6))

(defun winum-other-window-prefix-7 ()
  (interactive)
  (winum-other-window-prefix 7))

(defun winum-other-window-prefix-8 ()
  (interactive)
  (winum-other-window-prefix 8))

(defun winum-other-window-prefix-9 ()
  (interactive)
  (winum-other-window-prefix 9))


(defun winum-other-window-prefix-0 ()
  (interactive)
  (winum-other-window-prefix 10))

(define-key global-map (kbd "M-I 1") 'winum-other-window-prefix-1)
(define-key global-map (kbd "M-I 2") 'winum-other-window-prefix-2)
(define-key global-map (kbd "M-I 3") 'winum-other-window-prefix-3)
(define-key global-map (kbd "M-I 4") 'winum-other-window-prefix-4)
(define-key global-map (kbd "M-I 5") 'winum-other-window-prefix-5)
(define-key global-map (kbd "M-I 6") 'winum-other-window-prefix-6)
(define-key global-map (kbd "M-I 7") 'winum-other-window-prefix-7)
(define-key global-map (kbd "M-I 8") 'winum-other-window-prefix-8)
(define-key global-map (kbd "M-I 9") 'winum-other-window-prefix-9)
(define-key global-map (kbd "M-I 0") 'winum-other-window-prefix-0)
