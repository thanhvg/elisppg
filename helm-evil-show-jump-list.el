(evil-define-command evil-show-jumps ()
  "Display the contents of the jump list."
  :repeat nil
  (evil-with-view-list
    :name "evil-jumps"
    :mode "Evil Jump List"
    :format [("Jump" 5 nil)
             ("Marker" 8 nil)
             ("File/text" 1000 t)]
    :entries (let* ((jumps (evil--jumps-savehist-sync))
                    ;; ((229
                    ;; "/home/thanh/git/elisppg/helm-evil-show-jump-list.el")
                    ;; (538
                    ;; "/home/thanh/git/elisppg/helm-evil-show-jump-list.el")
                    ;; (650
                    ;; "/home/thanh/git/elisppg/helm-evil-show-jump-list.el")
                    ;; (427
                    ;; "/home/thanh/git/elisppg/helm-evil-show-jump-list.el")
                    ;; (25889 "/home/thanh/org/dotfiles/.spacemacs") (28094
                    ;; "/home/thanh/org/dotfiles/.spacemacs") (28111
                    ;; "/home/thanh/org/dotfiles/.spacemacs") (427
                    ;; "/home/thanh/git/elisppg/helm-evil-show-jump-list.el")
                    ;; (2376 "/home/thanh/git/elisppg/helm-evil-marks.el") (3706
                    ;; "/home/thanh/git/elisppg/helm-evil-marks.el") (28196
                    ;; "/home/thanh/.emacs.d/elpa/28.0/develop/evil-20200417.1238/evil-commands.el")
                    ;; (4096 "/home/thanh/git/elisppg/helm-evil-marks.el") (298
                    ;; "/home/thanh/git/elisppg/helm-evil-show-jump-list.el"))
                    (count 0))
               (cl-loop for jump in jumps
                        collect `(nil [,(number-to-string (cl-incf count))
                                       ,(number-to-string (car jump))
                                       (,(cadr jump))])))
    :select-action #'evil--show-jumps-select-action))

(defun helm-evil-show-jumps-alist ()
  "Return list of ('buffer-name:line-number:line-content' . (mark file-name))."
  (mapcar (lambda (jump)
            ;; don't use file-file which will switch to buffer
            (with-current-buffer (find-file-noselect (elt jump 1))
              (save-excursion
                (goto-char (car jump))
                (cons
                 (format "%s:%s: %s"
                         (buffer-name)
                         (line-number-at-pos)
                         (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
                 (list jump)))))
          (evil--jumps-savehist-sync)))


(defun helm-evil--show-jumps-select-action (jump)
  "Simlar to `evil--show-jumps-select-action' but no kill buffer."
  (let ((position (car jump))
        (file (elt jump 1)))
    (switch-to-buffer (find-file file))
    (goto-char position)))

(defun helm-evil-show-jumps ()
  "List evil markers with helm."
  (interactive)
  (helm :sources (helm-build-sync-source "Evil Jumps"
                   :candidates (helm-evil-show-jumps-alist)
                   :action (lambda (candidate)
                             ;; (message "%s" candidate)))
                             (helm-evil--show-jumps-select-action (car candidate))))
        :buffer "*helm-evil-jumps*"))

(helm-evil-show-jumps-alist)
(helm-evil-show-jumps)


