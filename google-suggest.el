;; -*- lexical-binding: t -*-
;;
;; google and stackexchange integration
;; https://www.reddit.com/r/emacs/comments/e8cm8x/get_stackoverflow_answers_with_completion_without/

(require 'json)
(require 'cl)
(require 'url)

(defun google-get-completions (query callback)
  (url-retrieve (format "http://suggestqueries.google.com/complete/search?client=chrome&q=%s" query)
                callback))

(defvar google-bufname "*stackexchange-suggestions*")

(defvar google-keymap
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map minibuffer-local-map)
        (define-key map (kbd "C-k") 'google-prev)
        (define-key map (kbd "C-j") 'google-next)
        (define-key map (kbd "<RET>") 'exit-minibuffer)
        map))

(defvar google-input nil)

(defun google-search ()
  (interactive)
  (let ((wincfg (current-window-configuration)))
    (pop-to-buffer google-bufname)
    (erase-buffer)
    (setq cursor-type nil)
    (add-hook 'post-command-hook 'google-post-command)
    (setq google-input nil)
    (if (unwind-protect
            (progn
              (read-from-minibuffer "search for: " nil google-keymap)
              (and google-input
                   (not (equal google-input ""))))

          (remove-hook 'post-command-hook 'google-post-command)
          (set-window-configuration wincfg))
        (message google-input))))

(defun google-post-command ()
  (if (and (not (equal google-input (minibuffer-contents)))
           (sit-for 0.3))

      (if (equal (minibuffer-contents) "")
          (with-current-buffer (get-buffer google-bufname)
            (erase-buffer))

        (let ((input (minibuffer-contents)))
          (google-get-completions
           input
           (lambda (status &rest args)
             (unless status
               (search-forward "\n\n")
               (let ((suggestions (append (aref (json-read) 1) nil)))
                 (with-current-buffer (get-buffer google-bufname)
                   (erase-buffer)
                   (insert input "\n")
                   (dolist (suggestion suggestions)
                     (insert suggestion "\n"))
                   (goto-char (point-min))
                   (google-highlight-line)
                   (setq google-input input))))))))))

(defun google-highlight-line ()
  (put-text-property (line-beginning-position)
                     (1+ (line-end-position))
                     'face
                     'highlight))


(defun google-clear-highlight ()
  (put-text-property (line-beginning-position)
                     (1+ (line-end-position))
                     'face
                     'nil))

(defun google-next ()
  (interactive)
  (with-current-buffer (get-buffer google-bufname)
    (unless (or (eobp)
                (save-excursion
                  (forward-line 1)
                  (eobp)))
      (google-clear-highlight)
      (forward-line 1)
      (google-item-selected))))


(defun google-prev ()
  (interactive)
  (with-current-buffer (get-buffer google-bufname)
    (unless (bobp)
      (google-clear-highlight)
      (forward-line -1)
      (google-item-selected))))


(defun google-item-selected ()
  (let ((item (with-current-buffer (get-buffer google-bufname)
                (google-highlight-line)
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))))
    (select-window (minibuffer-window))
    (delete-minibuffer-contents)
    (insert item)
    (setq google-input item)))
