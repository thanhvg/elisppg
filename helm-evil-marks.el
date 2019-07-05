;; draft and ripoff
(defun helm-evil-marks-alist ()
  (let ((buffer (current-buffer))
        (my-list))
    (dolist (element evil-markers-alist my-list)
      (let* ((code (car element))
             (char (byte-to-string code))
             (marker (cdr element)))
        (if (markerp marker)
            (let ((pos (marker-position marker)))
              (save-excursion
                (goto-char pos)
                (add-to-list
                 'my-list
                 (cons (format "%s> %s" char
                               (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
                       pos)))))))))

(defun helm-evil-marks-sort (candidates _source)
  "Custom sorting for matching CANDIDATES from SOURCE."
  (let ((pattern helm-pattern))
    (if (string= pattern "")
        candidates
      (sort candidates
            (lambda (_s1 s2)
              (if (string-prefix-p (format "%s> " pattern) (car s2))
                  nil
                t))))))

(defun helm-evil-marks ()
  "List evil markers with helm."
  (interactive)
  (helm :sources (helm-build-sync-source "Evil Markers"
                   :candidates (helm-evil-marks-alist)
                   :action (lambda (candidate)
                             (goto-char candidate))
                   :filtered-candidate-transformer #'helm-evil-marks-sort)
        :buffer "*helm-evil-marks*"))

(defun helm-evil-show-marks ()
  (interactive)
  (let ((all-markers
         ;; get global and local marks
         (append (cl-remove-if (lambda (m)
                                 (or (evil-global-marker-p (car m))
                                     (not (markerp (cdr m)))))
                               evil-markers-alist)
                 (cl-remove-if (lambda (m)
                                 (or (not (evil-global-marker-p (car m)))
                                     (not (markerp (cdr m)))))
                               (default-value 'evil-markers-alist)))))
    ;; map marks to list of 4-tuples (char row col file)
    (setq all-markers
          (mapcar (lambda (m)
                    (with-current-buffer (marker-buffer (cdr m))
                      (save-excursion
                        (goto-char (cdr m))
                        (list (car m)
                              (line-number-at-pos (point))
                              (current-column)
                              (buffer-name)))))
                  all-markers))))
;; => ((91 40 0 "helm-evil-marks.el") (93 40 15 "helm-evil-marks.el") (94 40 15
;; "helm-evil-marks.el") (97 12 0 "helm-evil-marks.el") (98 9 0
;; "helm-evil-marks.el") (99 30 20 "helm-evil-marks.el") (65 8 0
;; "indent-motion.el") (71 30 24 "helm-evil-marks.el"))



;; working 
(defun helm-evil-marks-show-alist ()
  (let ((all-markers
         ;; get global and local marks
         (append (cl-remove-if (lambda (m)
                                 (or (evil-global-marker-p (car m))
                                     (not (markerp (cdr m)))))
                               evil-markers-alist)
                 (cl-remove-if (lambda (m)
                                 (or (not (evil-global-marker-p (car m)))
                                     (not (markerp (cdr m)))))
                               (default-value 'evil-markers-alist)))))
    ;; map marks to cons (marker-buffer-line . marker)
    (mapcar (lambda (m)
              (with-current-buffer (marker-buffer (cdr m))
                (save-excursion
                  (goto-char (cdr m))
                  (cons
                   (format "%s>%s:: %s"
                           (char-to-string (car m))
                           (buffer-name)
                           (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
                   (car m)))))
            all-markers)))


(defun helm-evil-marks-show ()
  "List evil markers with helm."
  (interactive)
  (helm :sources (helm-build-sync-source "Evil Markers"
                   :candidates (helm-evil-marks-show-alist)
                   :action (lambda (candidate)
                             (evil-goto-mark candidate)))
        :buffer "*helm-evil-marks*"))
