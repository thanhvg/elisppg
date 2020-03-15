;;; -*- lexical-binding: t -*-

;; define handle
(defun xref-tide-xref-backend ()
  "Xref-Js2 backend for Xref."
  'xref-tide)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-tide)))
  (tide-get-symbol-at-point))

(cl-defmethod xref-backend-references ((_backend (eql xref-tide)) symbol)
  (tide-xref--find-references))

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
                                        0))))

(defun tide-xref--find-references ()
  "Return references."
  (let ((response (tide-command:references)))
    (tide-on-response-success response
        (let ((references (tide-plist-get response :body :refs)))
          (-map #'tide-xref--make-reference references)))))


(add-hook 'tide-mode-hook (lambda ()
                            (add-hook 'xref-backend-functions #'xref-tide-xref-backend nil t)))
