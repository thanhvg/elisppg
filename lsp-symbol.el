;;; lsp-mode.el --- LSP mode                              -*- lexical-binding: t; -*-
(defun lsp-thanh-symbol ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (unless (lsp--capability "documentHighlightProvider")
    (signal 'lsp-capability-not-supported (list "documentHighlightProvider")))
  (lsp--thanh-symbol (point)))


(defun lsp--thanh-symbol (my-point)
  (lsp-request-async "textDocument/documentHighlight"
                     (lsp--text-document-position-params)
                     (lsp--make-thanh-symbol-callback-2 my-point)))



(defun lsp--make-thanh-symbol-callback (buf)
  "thanh test"
  (cl-check-type buf buffer)
  (lambda (highlights)
    (message "length of %S: "  (length highlights))
    (seq-doseq (highlight highlights)
      (let* ((range (gethash "range" highlight nil))
             (kind (gethash "kind" highlight 1))
             (start (gethash "start" range))
             (end (gethash "end" range)))
        (progn
          ;; (message "highlight: %S "  highlight)
          ;; (message "range: %S "  range)
          ;; (message "kind: %S "  kind)
          (message "start: %S "  start)
          (message "end: %S "  end)
          (message "point: %S " (lsp--position-to-point start)))))))



(defun lsp--make-thanh-symbol-callback-2 (my-point)
  "thanh test"
  (lambda (highlights)
    ;; map highlights to a point list
    (let* ((my--points (-map (lambda (it)
                               (lsp--position-to-point (gethash "start" (gethash "range" it nil)))) highlights))
           (points (-sort '< my--points))
           (next-point (-first (lambda (it) (> it my-point)) points)))
      (message "point %S: "  next-point)
      (goto-char next-point))))
