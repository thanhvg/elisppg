;;  -*- lexical-binding: t; -*-

;; a blantant copy of https://git.sr.ht/~iank/spray

(require 'centered-cursor-mode)

(defvar quickread--running nil)
(defvar quickread--delay 0)
(defvar quickread-wpm 600)

(defvar quickread-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'quickread-quit)
    (define-key map (kbd "s") 'quickread-start/stop)
    (define-key map (kbd "h") 'quickread-backward-word)
    (define-key map (kbd "l") 'quickread-forward-word)
    (define-key map (kbd "<left>") 'quickread-backward-word)
    (define-key map (kbd "<right>") 'quickread-forward-word)
    (define-key map (kbd "u") 'quickread-faster)
    (define-key map (kbd "d") 'quickread-slower)
    (define-key map (kbd "t") 'quickread-time)
    map))

(defvar quickread--accent-overlay nil)
(defvar quickread--saved-cursor-type nil)

;; * faces

(defface quickread-base-face
  '((t (:inherit default)))
  "Face for non-accent characters."
  :group 'quickread)

(defface quickread-accent-face
  '((t (:foreground "red" :inherit quickread-base-face)))
  "Face for accent character."
  :group 'quickread)

(define-minor-mode quickread-mode
  "quickread mode"
  :lighter " Quickread"
  :init nil
  :keymap quickread-mode-map
  (if quickread-mode
      ;; on
      (progn
        (setq quickread--accent-overlay (make-overlay 0 0)
              quickread--saved-cursor-type cursor-type)
        (setq cursor-type nil)
        (overlay-put quickread--accent-overlay 'priority 101)
        (overlay-put quickread--accent-overlay 'face 'highlight)
        (quickread-start))

    ;; off
    (quickread-stop)
    (delete-overlay quickread--accent-overlay)
    (setq cursor-type quickread--saved-cursor-type)))

(defun quickread-start ()
  (when quickread--running
    (cancel-timer quickread--running))

  (setq quickread--running
        (run-with-timer 0 (/ 60.0 quickread-wpm) 'quickread--update)))


(defun quickread-stop ()
  (when quickread--running
    (cancel-timer quickread--running)
    (setq quickread--running nil)))

(defun quickread-quit ()
  "Exit quickread mode."
  (interactive)
  (quickread-mode -1))

(defun quickread--update ()
  (cond ((not (zerop quickread--delay))
         (setq quickread--delay (1- quickread--delay)))
        (t
         (if (eobp)
             (quickread-quit)
           (skip-chars-forward "\s\t\n—")
           (quickread--word-at-point)))))

(defun quickread--word-at-point ()
  (skip-chars-backward "^\s\t\n—")
  (let* ((beg (point))
         (len (+ (skip-chars-forward "^\s\t\n—") (skip-chars-forward "—")))
         (end (point))
         (accent (+ beg (cl-case len
                          ((1) 1)
                          ((2 3 4 5) 2)
                          ((6 7 8 9) 3)
                          ((10 11 12 13) 4)
                          (t 5)))))
    (setq quickread--delay (+
                            (round len 5)
                            (if (looking-at "\n[\s\t\n]") 3 0)
                            (cl-case (char-before)
                              ((?. ?! ?\? ?\;) 3)
                              ((?, ?: ?—) 1)
                              (t 0))))
    (move-overlay quickread--accent-overlay (1- accent) accent)
    (ccm-position-cursor)))

(defun quickread-start/stop ()
  "Toggle pause/unpause quickread."
  (interactive)
  (if quickread--running
      (quickread-stop)
    (quickread-start)))


(defun quickread-forward-word ()
  (interactive)
  (quickread-stop)
  (skip-chars-forward "\s\t\n—")
  (quickread--word-at-point))

(defun quickread-backward-word ()
  (interactive)
  (quickread-stop)
  (skip-chars-backward "^\s\t\n—")
  (skip-chars-backward "\s\t\n—")
  (quickread--word-at-point))

(defun quickread-faster ()
  "Increases speed.

Increases the wpm (words per minute) parameter. See the variable
`quickread-wpm'."
  (interactive)
  (quickread-inc-wpm 20))

(defun quickread-slower ()
  "Decreases speed.

Decreases the wpm (words per minute) parameter. See the variable
`quickread-wpm'."
  (interactive)
  (quickread-inc-wpm -20))

(defun quickread-inc-wpm (delta)
  (let ((was-running quickread--running))
    (quickread-stop)
    (when (< 10 (+ quickread-wpm delta))
      (setq quickread-wpm (+ quickread-wpm delta)))
    (and was-running (quickread-backward-word))
    (message "spray wpm: %d" quickread-wpm)
    (when was-running
      (quickread-start))))

(defun quickread-time ()
  (interactive)
  (let ((position (progn (skip-chars-backward "^\s\t\n—") (point))))
    (message
     "%d per cent done; ~%d minute(s) remaining"
     (* 100 (/ position (+ 0.0 (point-max))))
     (fround (/ (count-words-region position (point-max)) (+ 0.0 quickread-wpm)))))
  (quickread--word-at-point))

(provide 'quickread)

;; spacemacs settings

(spacemacs/set-leader-keys "arb" 'speed-reading/start-quickread)

(defun speed-reading/start-quickread ()
  "Start quickread speed reading on current buffer at current point."
  (interactive)
  (evil-normal-state)
  (quickread-mode t)
  (internal-show-cursor (selected-window) nil))

(map-keymap (lambda (event function)
              (evil-define-minor-mode-key 'normal 'quickread-mode (vector event) function))
            quickread-mode-map)

;; (evil-define-minor-mode-key 'normal 'quickread-mode  "q" 'quickread-quit)
;; (evil-define-minor-mode-key 'normal 'quickread-mode  "s" 'quickread-start/stop)
;; (evil-define-minor-mode-key 'normal 'quickread-mode  "h" 'quickread-backward-word)
;; (evil-define-minor-mode-key 'normal 'quickread-mode  "l" 'quickread-forward-word)
;; (evil-define-minor-mode-key 'normal 'quickread-mode  "u" 'quickread-faster)
;; (evil-define-minor-mode-key 'normal 'quickread-mode  "d" 'quickread-slower)
;; (evil-define-minor-mode-key 'normal 'quickread-mode  "t" 'quickread-time)

(defun spacemacs//restore-cursor ()
  (unless quickread-mode
    (internal-show-cursor (selected-window) t)))

(add-hook 'quickread-mode-hook #'spacemacs//restore-cursor)
