;;; -*- lexical-binding: t; -*-
(defun howdoyou-google-to-links (dom)
  (let* ((my-divs (dom-by-class dom "jfp3ef"))
         (my-a-tags (mapcar (lambda (a-div)
                              (dom-attr (dom-child-by-tag a-div 'a) 'href))
                            my-divs)))
    (mapcar (lambda (it) (substring it 7))
            (seq-filter (lambda (it) (if it t nil)) my-a-tags))))

(defun howdoyou-google-search (query)
  (let ((url-request-method "GET")
        (url "https://www.google.com/search")
        (args (concat "?q="
                      (url-hexify-string "site:stackoverflow.com ")
                      (url-hexify-string query))))
    (message "%s" args)
    (url-retrieve (concat url args)
                  (lambda (status)
                    (switch-to-buffer (current-buffer))
                    (let* ((dom (libxml-parse-html-region (point-min) (point-max))))
                      (erase-buffer)
                      (setq thanh dom)))
                  nil t t)))

(aio-defun howdoyou-google-dom (query)
  (let* ((url (concat "https://www.google.com/search?q="
                      (url-hexify-string "site:stackoverflow.com ")
                      (url-hexify-string query)))
         ;; (_me (message "%s" url))
         (my-web (aio-await (aio-url-retrieve url)))
         (contents (with-current-buffer (cdr my-web)
                     (prog1  (libxml-parse-html-region (point-min) (point-max))
                       (kill-buffer)))))
    (howdoyou-google-to-links contents)))

(aio-defun howdoyou-so-links (query)
  (let ((links (aio-await (howdoyou-google-dom query))))
    (message "%s" links)))

(aio-defun fetch-fortune-3 (url)
  (let ((buffer (aio-await (aio-url-retrieve url))))
    (with-current-buffer buffer
      (prog1 (buffer-string)
        (kill-buffer)))))

(aio-defun foo (url2)
  (let* ((url "https://google.com")
         (result (aio-await (aio-url-retrieve url)))
         (_me (message "%s" url))
         (contents (with-current-buffer (cdr result)
                     (prog1 (buffer-string)
                       (kill-buffer)))))
    (message "Result: got it")))


;; (aio-defun foo-3 (query)
;;   (let* ((url (concat "https://www.google.com/search?q="
;;                       (url-hexify-string "site:stackoverflow.com ")
;;                       (url-hexify-string query))))
;;     (message "%s" url)))
