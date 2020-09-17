(defun +ivy-format-function-line-or-arrow (cands)
  "Transform CANDS into a string for minibuffer.

If in terminal, prefix candidates with a chevron to make it more obvious which
one you're selecting, especially in themes that can't set a good background for
`ivy-current-match'. This is a combination of `ivy-format-function-line' and
`ivy-format-function-arrow'.

In the GUI, this is the same as `ivy-format-function-line'."
  (if (display-graphic-p)
      (ivy-format-function-line cands)
    (ivy--format-function-generic
     (lambda (str)
       (ivy--add-face (concat "> " str "\n") 'ivy-current-match))
     (lambda (str)
       (concat "  " str "\n"))
     cands
     "")))

(setq ivy-rich-parse-remote-buffer nil)

(defmacro plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
            on (list ,@rest) by #'cddr
            do ,(if (symbolp plist)
                    `(setq ,plist (plist-put ,plist prop value))
                  `(plist-put ,plist prop value))))


;; Highlight each ivy candidate including the following newline, so that it
;; extends to the right edge of the window
(setf (alist-get 't ivy-format-functions-alist)
      #'+ivy-format-function-line-or-arrow)


  ;; Enahnce the appearance of a couple counsel commands
  (plist-put! ivy-rich-display-transformers-list
              'counsel-describe-variable
              '(:columns
                ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                 (+ivy-rich-describe-variable-transformer (:width 50)) ; display variable value
                 (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
              'counsel-M-x
              '(:columns
                ((counsel-M-x-transformer (:width 60))
                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
              ;; Apply switch buffer transformers to `counsel-projectile-switch-to-buffer' as well
              'counsel-projectile-switch-to-buffer
              (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)
              'counsel-bookmark
              '(:columns
                ((ivy-rich-candidate (:width 0.5))
                 (ivy-rich-bookmark-filename (:width 60)))))

  ;; Remove built-in coloring of buffer list; we do our own
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)

  ;; Highlight buffers differently based on whether they're in the same project
  ;; as the current project or not.
  (when-let* ((plist (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer))
              (switch-buffer-alist (assq 'ivy-rich-candidate (plist-get plist :columns))))
    (setcar switch-buffer-alist '+ivy-rich-buffer-name))


(defvar ivy-rich--ivy-switch-buffer-cache
  (make-hash-table :test 'equal))

(define-advice ivy-rich--ivy-switch-buffer-transformer
    (:around (old-fn x) cache)
  (let ((ret (gethash x ivy-rich--ivy-switch-buffer-cache)))
    (unless ret
      (setq ret (funcall old-fn x))
      (puthash x ret ivy-rich--ivy-switch-buffer-cache))
    ret))

(define-advice +ivy/switch-buffer
    (:before (&rest _) ivy-rich-reset-cache)
  (clrhash ivy-rich--ivy-switch-buffer-cache))


;; https://github.com/abo-abo/swiper/issues/2412
(setq ivy-format-functions-alist '((t . ivy-format-function-line)))

(setf (alist-get 't ivy-format-functions-alist)
      #'ivy-format-function-line)

;; (set-face-extend 'ivy-highlight-face t)
;; (set-face-extend 'ivy-prompt-match t)

