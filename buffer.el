;; make buffer list
(buffer-list)

(defun spacemacs//get-recent-buffers-3 ()
  (seq-remove #'spacemacs//layout-not-contains-buffer-p (buffer-list)))

(spacemacs//get-recent-buffers)
(spacemacs//get-recent-buffers-2)
(spacemacs//get-recent-buffers-3)

(frame-parameter nil 'buffer-predicate)

(defun spacemacs//get-recent-buffers-2 ()
  (seq-filter (frame-parameter nil 'buffer-predicate) (buffer-list)))

(defun helm-buffer-p (buffer)
  (string-prefix-p "*helm"
                   (buffer-name buffer)))

(defun spacemacs//get-recent-buffers ()
  (seq-filter  (lambda (buff)
                 (and
                  (funcall (frame-parameter nil 'buffer-predicate) buff)
                  (not (helm-buffer-p buff))))
               (buffer-list)))


(defun spacemacs//switch-to-buff-by-pos (pos)
  (let ((my-buff (elt (spacemacs//get-recent-buffers) (+ pos 1))))
                 (message "buffer %s" (buffer-name my-buff)
                 (switch-to-buffer my-buff))))

;; (spacemacs//switch-to-buff-by-pos 2)

(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "spacemacs/switch-to-buff-%s" i)) nil
           ,(format "Switch to buffer %s.\n%s"
                    i "See `spacemacs//switch-to-buff-by-pos' for details.")
           (interactive)
           (spacemacs//switch-to-buff-by-pos ,(if (eq 0 i) 9 (1- i))))))


(defun spacemacs//buffers-ts-hint ()
  (let ((my-index 1)
        (my-string "")
        (my-buffer-list (cdr (spacemacs//get-recent-buffers)))
        (my-first-entry-done nil))
    (while (and (< my-index 10) my-buffer-list)
      (setq my-string (concat my-string
                              (if my-first-entry-done
                                  (format " | %s:" my-index)
                                (setq my-first-entry-done t)
                                (format " %s:" my-index))
                              (buffer-name (car my-buffer-list))))
      (incf my-index)
      (setq my-buffer-list (cdr my-buffer-list)))
    ;; (substring my-string 0 (- (frame-total-cols) 15))))
    my-string))

(spacemacs//buffers-ts-hint)

(spacemacs|transient-state-format-hint buffers
  spacemacs--buffers-ts-full-hint
  "\n
 [_0_.._9_]^^     nth/new layout    [_a_]^^   add buffer
 [_b_]^^^^        buffer in layout  [_r_]^^   remove current buffer
 ^^^^^^                             [_<_/_>_] move layout left/right
 ^^^^^^                             [_?_]^^   toggle help")

(spacemacs|define-transient-state buffers
  :title "Buffers Transient State"
  :hint-is-doc t
  :dynamic-hint (spacemacs//buffers-ts-hint)
  :bindings
  ;; need to exit in case number doesn't exist
  ;; ("?" spacemacs//buffers-ts-toggle-hint)
  ("1" spacemacs/switch-to-buff-1 :exit t)
  ("2" spacemacs/switch-to-buff-2 :exit t)
  ("3" spacemacs/switch-to-buff-3 :exit t)
  ("4" spacemacs/switch-to-buff-4 :exit t)
  ("5" spacemacs/switch-to-buff-5 :exit t)
  ("6" spacemacs/switch-to-buff-6 :exit t)
  ("7" spacemacs/switch-to-buff-7 :exit t)
  ("8" spacemacs/switch-to-buff-8 :exit t)
  ("9" spacemacs/switch-to-buff-9 :exit t)
  ("0" spacemacs/switch-to-buff-0 :exit t))

;; (spacemacs/switch-to-buff-2)
;; (elt (spacemacs//get-recent-buffers) 20)

(spacemacs/set-leader-keys "." 'spacemacs/buffers-transient-state/body)
