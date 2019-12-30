(defun thanh-helm-move-line-action (&optional arg)
  (let ((select (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (delete-minibuffer-contents)
      (set-text-properties 0 (length select)
                           nil select)
      (insert (helm-get-selection)))))

(defun thanh-helm-persistent-action (candidate)
  (with-selected-window (or (active-minibuffer-window)
                            (minibuffer-window))
    (delete-minibuffer-contents)
    ;; (set-text-properties 0 (length select)
    ;;                      nil select)
    (insert candidate)))

(defun thanh-helm-line-movement-keymap (helm-keymap name)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-keymap)
    (dolist (action '(next-line previous-line next-page previous-page
                                beginning-of-buffer end-of-buffer toggle-visible-mark))
      (let ((orig-fn (intern (format "helm-%s" action)))
            (new-fn (intern (format "helm-%s-%s" name action))))
        (defalias new-fn `(lambda (&optional arg)
                            (interactive "p")
                            (call-interactively ',orig-fn)
                            (thanh-helm-move-line-action arg))
          (format "Replacement of `%s' action for `helm-%s'.

  \(fn ARG)" orig-fn name))
        (define-key map `[remap ,orig-fn] new-fn)))
    map))


(defvar helm-google-suggetst-keymap-extra
  (thanh-helm-line-movement-keymap helm-map "suggest-extra"))

(setq helm-source-google-suggest
      (helm-build-sync-source "Google Suggest"
        :candidates (lambda ()
                      (funcall helm-google-suggest-default-function))
        :action 'helm-google-suggest-actions
        :volatile t
        :persistent-action #'thanh-helm-persistent-action 
        :keymap helm-google-suggetst-keymap-extra
        :requires-pattern 3))
