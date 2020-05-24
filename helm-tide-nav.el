;;; helm-tide-nav.el --- blah blah -*- lexical-binding: t -*-
(defun helm-tide-nav ()
  (interactive)
  (helm :sources 'helm-source-tide-nav
        :buffer "*helm tide nav*"))

(defun helm-tide-nav-fetch ()
  (with-helm-current-buffer ;; very important otherwise tide will run on a nil buffer and it will call global tsserver 
   (let ((response (tide-command:navto helm-pattern)))
     (tide-on-response-success response
         (when-let ((navto-items (plist-get response :body))
                    (cutoff (length (tide-project-root))))
           (setq navto-items (funcall tide-navto-item-filter navto-items))
           (seq-map (lambda (navto-item)
                      (cons
                       (format "%s: %s"
                               ;; (car (reverse (split-string (plist-get navto-item :file) "\\/")))
                               (substring (plist-get navto-item :file) cutoff)
                               (plist-get navto-item :name))
                       navto-item))
                    navto-items))))))

;;(:name ResizeQuality :kind type :isCaseSensitive :json-false :matchKind
;; prefix :file
;; /home/thanh/git/vpl-backend/node_modules/typescript/lib/lib.dom.d.ts :start
;; (:line 20027 :offset 1) :end (:line 20027 :offset 62) :kindModifiers declare)

(defvar helm-source-tide-nav
  (helm-build-sync-source "Project Symbols"
    :candidates #'helm-tide-nav-fetch
    :action #'tide-jump-to-filespan
    ;; :action (lambda (cdd) (message "%s" cdd))
    :volatile t
    :requires-pattern 3))
