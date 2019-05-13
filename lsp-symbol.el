(defun lsp-thanh-symbol ()
  "Highlight all relevant references to the symbol under point."
  (interactive)
  (unless (lsp--capability "documentHighlightProvider")
    (signal 'lsp-capability-not-supported (list "documentHighlightProvider")))
  (lsp--thanh-symbol))


(defun lsp--thanh-symbol ()
  (lsp-request-async "textDocument/documentHighlight"
                     (lsp--text-document-position-params)
                     (lsp--make-thanh-symbol-callback (current-buffer))))



(defun lsp--make-thanh-symbol-callback (buf)
  "thanh test"
  (cl-check-type buf buffer)
  (lambda (highlights)
    (seq-doseq (highlight highlights)
      (let* ((range (gethash "range" highlight nil))
             (kind (gethash "kind" highlight 1))
             (start (gethash "start" range))
             (end (gethash "end" range)))
        (progn
          (message "highlight: %S "  highlight)
          (message "range: %S "  range)
          (message "kind: %S "  kind)
          (message "start: %S "  start)
          (message "end: %S "  end))))))
