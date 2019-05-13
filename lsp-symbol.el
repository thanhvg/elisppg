;;;  -*- lexical-binding: t; -*-

(defun lsp-goto-symbol-occurence (&optional backward)
  (unless (lsp--capability "documentHighlightProvider")
    (signal 'lsp-capability-not-supported (list "documentHighlightProvider")))
  (lsp--goto-symbol-occurent (point) backward))


(defun lsp--goto-symbol-occurent (a-point backward)
  (lsp-request-async "textDocument/documentHighlight"
                     (lsp--text-document-position-params)
                     (lsp--make-goto-symbol-occurence-callback a-point backward)))

(defun lsp--make-goto-symbol-occurence-callback (a-point backward)
  (lambda (highlights)
    (when (> (length highlights) 1)
      ;; map highlights to a point list
      (let* ((my--points (-map (lambda (it)
                                 (lsp--position-to-point (gethash "start" (gethash "range" it nil)))) highlights))
             (points (-sort '< my--points))
             (len (length my--points))
             (goto--index (if backward (-find-last-index (lambda (it) (< it a-point)) points)
                            (-find-index (lambda (it) (> it a-point)) points)))
             (goto-index (if goto--index goto--index (if backward (- len 1) 0)))
             (goto-point (nth goto-index points)))
        (message "occurence %S/%S"  (+ goto-index 1) len)
        (goto-char goto-point)))))

(defun lsp-goto-symbol-occurence-forward ()
  (interactive)
  (lsp-goto-symbol-occurence))


(defun lsp-goto-symbol-occurence-backward ()
  (interactive)
  (lsp-goto-symbol-occurence t))
