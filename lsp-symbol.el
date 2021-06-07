;;;  -*- lexical-binding: t; -*-

(defun lsp-goto-symbol-occurence (direction)
  "DIRECTION is forward, backward, last, first."
  (unless (lsp--capability "documentHighlightProvider")
    (signal 'lsp-capability-not-supported (list "documentHighlightProvider")))
  (lsp--goto-symbol-occurent (point) direction))

(defun lsp--goto-symbol-occurent (a-point direction)
  (lsp-request-async "textDocument/documentHighlight"
                     (lsp--text-document-position-params)
                     (lsp--make-goto-symbol-occurence-callback a-point direction)))

(defun lsp--make-goto-symbol-occurence-callback (a-point direction)
  (lambda (highlights)
    (if (> (length highlights) 1)
        ;; map highlights to a point list
        (let* ((my--points (-map (lambda (it)
                                   (lsp--position-to-point (gethash "start" (gethash "range" it nil))))
                                 highlights))
               (points (-sort '< my--points))
               (len (length my--points))
               (goto--index (pcase direction
                              ('backward (-find-last-index (lambda (it) (< it a-point)) points))
                              ('forward  (-find-index (lambda (it) (> it a-point)) points))
                              ('first 0)
                              ('last (- len 1))))
               (goto-index (if goto--index
                               goto--index
                             (if (equal direction 'backward) (- len 1) 0)))
               (goto-point (nth goto-index points)))
          (message "occurence %S/%S"  (+ goto-index 1) len)
          (goto-char goto-point))
      (message "only one occurence"))))

(defun lsp-goto-symbol-occurence-forward ()
  (interactive)
  (lsp-goto-symbol-occurence 'forward))

(defun lsp-goto-symbol-occurence-last ()
  (interactive)
  (lsp-goto-symbol-occurence 'last))

(defun lsp-goto-symbol-occurence-first ()
  (interactive)
  (lsp-goto-symbol-occurence 'first))

(defun lsp-goto-symbol-occurence-backward ()
  (interactive)
  (lsp-goto-symbol-occurence 'backward))
