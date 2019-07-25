(defun howdoyou-google-to-links (dom)
  (let* ((my-divs (dom-by-class dom "jfp3ef"))
         (my-as (mapcar (lambda (a-div)
                          (dom-attr (dom-child-by-tag a-div 'a) 'href))
                        my-divs)))
    (mapcar (lambda (it) (substring it 7))
            (seq-filter (lambda (it) (if it t nil)) my-as))))

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
                    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                           (dom-sub (dom-by-class dom "r")))
                      ;; (erase-buffer)
                      (setq thanh dom)
                      ;; (message (dom-by-class dom "r"))
                      (shr-insert-document (car dom-sub))))
                  nil t t)))
