;;; -*- lexical-binding: t -*-

;; define handle
(defun xref-tide-xref-backend ()
  "Xref-tide backend for xref."
  'xref-tide)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-tide)))
  (tide-get-symbol-at-point))

(cl-defmethod xref-backend-references ((_backend (eql xref-tide)) symbol)
  (tide-xref--find-references))

;; (cl-defmethod xref-backend-definitions ((_backend (eql xref-tide)) symbol)
;;   (tide-xref--find-definitions))

(defun tide-xref--make-reference (reference)
  "Make xref object from RERERENCE."
  (message "thanh2: %s" reference)
  (let ((full-file-name (plist-get reference :file))
        (line-number (tide-plist-get reference :start :line))
        (line-text (plist-get reference :lineText))
        (start (1- (tide-plist-get reference :start :offset)))
        (end   (1- (tide-plist-get reference :end :offset))))
    (put-text-property start end 'face 'tide-match line-text)
    (xref-make line-text
               (xref-make-file-location full-file-name
                                        line-number
                                        start))))

(defun tide-xref--find-references ()
  "Return references."
  (let ((response (tide-command:references)))
    (tide-on-response-success response
        (let ((references (tide-plist-get response :body :refs)))
          (-map #'tide-xref--make-reference references)))))


;; this is not practical, since tide-command:definition is an async you must
;; wait on it to return and the outcome would be the same as the current
;; solution but it could be worse because of your blocking implementation.
;; (defun tide-xref--find-definitions ()
;;   "Return definitions."
;;   (let* (
;;          (response (tide-command:definition cb)))
;;     (tide-on-response-success response
;;         (when-let ((definitions (tide-plist-get response :body :refs)))
;;           (message "thanh3: %s" definitions)
;;           (-map #'tide-xref--make-reference definitions)))))


(add-hook 'tide-mode-hook (lambda ()
                            (add-hook 'xref-backend-functions #'xref-tide-xref-backend nil t)))
