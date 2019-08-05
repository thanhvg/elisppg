;;; -*- lexical-binding: t; -*-
(require 'aio)
(require 'promise)
(require 'dom)


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
    (howdoyou--google-to-links contents)))

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

(defun example12 ()
  "Example using `xml-retrieve'."
  (let ((wikipedia-url (concat "https://en.wikipedia.org/w/api.php"
                               "?format=xml&action=query&prop=extracts"
                               "&exintro=&explaintext=&titles=")))
    (promise-chain (promise-all
                    (vector
                     (xml-retrieve (concat wikipedia-url (url-encode-url "GNU")))
                     ;; Request after 2 seconds for load reduction.
                     (wait-seconds 2
                                   #'xml-retrieve
                                   (concat wikipedia-url (url-encode-url "Emacs")))))
      (then (lambda (xmls)
              (message "%s" (get-short-text-first-tag (aref xmls 0) 'extract))
              (message "%s" (get-short-text-first-tag (aref xmls 1) 'extract))))

      (promise-catch (lambda (reason)
                       (message "promise-catch: %s" reason))))))
(defun xml-retrieve (url)               ; Same as `promise:xml-retrieve'
  "Return `Promise' to resolve with XML object obtained by HTTP request."
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
                               (search-forward-regexp "\n\\s-*\n" nil t)
                               (funcall resolve (xml-parse-region))))
                         (error (funcall reject ex)))))))))

(defun wait-seconds (seconds fn &rest args) ; Same as `promise:run-at-time'
  "Return `Promise' to execute the function after the specified time."
  (promise-new (lambda (resolve _reject)
                 (run-at-time seconds
                              nil
                              (lambda ()
                                (funcall resolve (apply fn args)))))))
(defun get-text-first-tag (xml tag)
  "Returns the first text that matches TAG in XML."
  (decode-coding-string (dom-text (cl-first (dom-by-tag xml tag)))
                        'utf-8))

(defun get-short-text-first-tag (xml tag)
  "Truncate the text obtained with `get-text-first-tag'."
  (concat (truncate-string-to-width (get-text-first-tag xml tag) 64)
          " ..."))



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
                      (url-hexify-string "site:stackoverflow.com ")
                      (url-hexify-string query))))
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
    nil))

(defun howdoyou--promise-so-answer (dom)
  "get the first child in class answers"
  (message "got the dom")
  ;; (setq thanh dom)
  (let* ((answer-dom (car (dom-by-class dom "^answer\s+"))))
    (message "yay")
    (dom-by-class answer-dom "post-text")))
    ;; (dom-texts (dom-by-class answer-dom "post-text"))))

(defun howdoyou--print-answer (answer)
  "print answer to buffer"
  (let ((howdoi-buffer (get-buffer-create "*How Do You*")))
    (save-selected-window
      (with-current-buffer howdoi-buffer
        (read-only-mode -1)
        (erase-buffer)
        ;; (insert answer)
        (shr-insert-document answer)
        (eww-mode)
        (goto-char (point-min)))
      (pop-to-buffer howdoi-buffer))))
