;;;  -*- lexical-binding: t; -*-

;;; lsp symbol overlay

;; get list of symbol occurences then put hightlight face on them

(defvar-local lsp-sov--hl-faces '(symbol-overlay-face-1
                                  symbol-overlay-face-2
                                  symbol-overlay-face-3
                                  symbol-overlay-face-4
                                  symbol-overlay-face-5
                                  symbol-overlay-face-6
                                  symbol-overlay-face-7
                                  symbol-overlay-face-8))

(defun lsp-sov--get-next-face ()
  (when (null lsp-sov--hl-faces)
    (signal 'lsp-sov "crap"))
  (let ((face (car lsp-sov--hl-faces)))
    (setq lsp-sov--hl-faces (cdr lsp-sov--hl-faces))
    face))

(defun lsp-sov-get-highlights ()
  (lsp-request-async "textDocument/documentHighlight"
                     (lsp--text-document-position-params)
                     #'lsp-sov--highlight-callback))

(defun lsp-sov--highlight-callback (highlights)
  (let ((face (lsp-sov--get-next-face)))
    (-each highlights
     (lambda (it)
       (let* ((range (gethash "range" it nil))
              (start (lsp--position-to-point (gethash "start" range)))
              (end (lsp--position-to-point (gethash "end" range))))
         (-doto (make-overlay start end)
           ;; (overlay-put 'face 'symbol-overlay-face-1)
           (overlay-put 'face face)
           (overlay-put 'lsp-sov t)))))))

(defun lsp-sov-do-hl ()
  (interactive)
  (lsp-sov-get-highlights))

(defun lsp-sov--overlayp (ov)
  (overlay-get ov 'lsp-sov))

(defun lsp-sov--clear ()
  "Clear all overlays in current buffer."
  (mapc #'delete-overlay
        (lsp-sov--get-buffer-overlays))
  (setq lsp-sov--hl-faces '(symbol-overlay-face-1
                                    symbol-overlay-face-2
                                    symbol-overlay-face-3
                                    symbol-overlay-face-4
                                    symbol-overlay-face-5
                                    symbol-overlay-face-6
                                    symbol-overlay-face-7
                                    symbol-overlay-face-8)))

(defun lsp-sov-clear-all ()
  (interactive)
  (lsp-sov--clear))

(defun lsp-sov--get-buffer-overlays (&optional should-sort)
  (let ((ovs (seq-filter #'lsp-sov--overlayp
                         (overlays-in (point-min) (point-max)))))
    (if should-sort
        (sort ovs
              (lambda (first second)
                (< (overlay-start first) (overlay-start second))))
      ovs)))
