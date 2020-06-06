;; make buffer list

(spacemacs//get-recent-buffers)

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
        (my-first-entry-done nil)
        (my-max-len (- (frame-total-cols) 45))
        (my-b-key " [b] list-buffers"))
    (add-text-properties 2 3 '(face hydra-face-blue) my-b-key)
    (while (and (< my-index 10) my-buffer-list)
      (setq my-string (concat my-string
                              (if my-first-entry-done
                                  (format " | %s:" (propertize (format "%s" my-index) 'face 'hydra-face-blue))
                                (setq my-first-entry-done t)
                                (format " %s:" (propertize (format "%s" my-index) 'face 'hydra-face-blue)))
                              (buffer-name (car my-buffer-list))))
      (incf my-index)
      (setq my-buffer-list (cdr my-buffer-list)))
    ;; (substring my-string 0 (- (frame-total-cols) 15))))
    (if (< (length my-string) my-max-len)
        (concat my-string my-b-key)
      (concat (substring my-string 0 (- my-max-len 3))
              "..." my-b-key))))

(spacemacs//buffers-ts-hint)

(spacemacs|define-transient-state buffers
  :title "Buffers Transient State"
  :hint-is-doc t
  :dynamic-hint (spacemacs//buffers-ts-hint)
  :bindings
  ("b" lazy-helm/helm-mini :exit t)
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
