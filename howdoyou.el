;;; -*- lexical-binding: t; -*-
(require 'promise)
(require 'dom)

(defun howdoyou--google-to-links (dom)
  (let* ((my-divs (dom-by-class dom "jfp3ef"))
         (my-a-tags (mapcar (lambda (a-div)
                              (dom-attr (dom-child-by-tag a-div 'a) 'href))
                            my-divs)))
    (mapcar (lambda (it) (substring it 7))
            (seq-filter (lambda (it) (if it t nil)) my-a-tags))))

(defun howdoyou--promise-dom (url)
  (promise-new
   (lambda (resolve reject)
     (url-retrieve url
                   (lambda (status)
                     ;; All errors are reliably captured and rejected with appropriate values.
                     (if (plist-get status :error)
                         (funcall reject (plist-get status :error))
                       (condition-case ex
                           (with-current-buffer (current-buffer)
                             (if (not (url-http-parse-headers))
                                 (funcall reject (buffer-string))
                               ;; (message "%s" (buffer-string))
                               ;; (message "got it")
                               (funcall resolve (libxml-parse-html-region (point-min) (point-max)))))
                         (error (funcall reject ex)))))))))

(defun howdoyou-promise-answer (query)
  "query and print answer"
  (let ((url "https://www.google.com/search")
        (args (concat "?q="
                      (url-hexify-string query)
                      (url-hexify-string " ")
                      (url-hexify-string "site:stackoverflow.com OR ")
                      (url-hexify-string "site:stackexchange.com OR ")
                      (url-hexify-string "site:superuser.com OR ")
                      (url-hexify-string "site:serverfault.com OR ")
                      (url-hexify-string "site:askubunu.com"))))
    (promise-chain (howdoyou--promise-dom (concat url args))
      (then (lambda (dom)
              (let ((my-link (howdoyou--google-to-links dom)))
                (promise-resolve my-link))))
      (then (lambda (links)
              ;; (message "%s" links)
              ;; (setq thanh links)
              (howdoyou--promise-dom (car links))))
      (then #'howdoyou--promise-so-answer)
      (then #'howdoyou--print-answer))
    (concat url args)))

(defun howdoyou--promise-so-answer (dom)
  "get the first child in class answers"
  (message "got the dom")
  ;; (setq thanh dom)
  (let ((answer-dom (car (dom-by-class dom "^answer\s?")))
        (question-dom (car (dom-by-id dom "^question$"))))
    (list (dom-by-class question-dom "post-text")
          (dom-by-class answer-dom "post-text"))))
;; (dom-texts (dom-by-class answer-dom "post-text"))))

(defun howdoyou--print-answer (answer)
  "print answer to buffer"
  (let ((howdoi-buffer (get-buffer-create "*How Do You*")))
    (save-selected-window
      (with-current-buffer howdoi-buffer
        (read-only-mode -1)
        (erase-buffer)
        ;; (insert answer)
        (shr-insert-document (car answer))
        (shr-insert "==================Answer==================")
        (shr-insert-document (nth 1 answer))
        (eww-mode)
        (goto-char (point-min)))
      (pop-to-buffer howdoi-buffer))))
