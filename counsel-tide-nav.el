(defun counsel-tide-nav (&optional initial-input)
  (interactive)
  (ivy-read "Tide project symbol: "
            #'counsel-tide-nav--fn
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-tide-nav-history
            :require-match t
            :action (lambda (x) (tide-jump-to-filespan (counsel-tide-nav--get-value x)))
            :caller 'counsel-tide-nav))

(defun counsel-tide-nav--make-data (name value)
  (let ((my-hash (make-hash-table :test 'equal)))
    (puthash "name" name my-hash)
    (puthash "value" value my-hash)
    my-hash))

(defun counsel-tide-nav--get-name (data)
  (gethash "name" data))

(defun counsel-tide-nav--get-value (data)
  (gethash "value" data))

(ivy-configure 'counsel-tide-nav
  :display-transformer-fn
  #'counsel-tide-nav--get-name)

(defun counsel-tide-nav--fn (str)
  (let ((response (tide-command:navto str)))
    (tide-on-response-success response
        (when-let ((navto-items (plist-get response :body))
                   (cutoff (length (tide-project-root))))
          (setq navto-items (funcall tide-navto-item-filter navto-items))
          (seq-map (lambda (navto-item)
                     (counsel-tide-nav--make-data
                      (format "%s: %s"
                              (substring (plist-get navto-item :file) cutoff)
                              (plist-get navto-item :name))
                      navto-item))
                   navto-items)))))


(defun counsel-foo ()
  (interactive)
  (ivy-read "Foo: "
            (counsel-foo-fn "x")
            :action (lambda (x) (message "I got %s" x))))


(defun counsel-foo-dynamic ()
  (interactive)
  (ivy-read "Foo dyn: "
            #'counsel-foo-fn
            :dynamic-collection t
            :action (lambda (x) (message "I got %s" x))
            :caller 'counsel-foo-dynamic))

(ivy-configure 'counsel-foo-dynamic
  :display-transformer-fn
  (lambda (elt) (gethash "val" elt)))

(setq myhash (make-hash-table :test 'equal))

(puthash "1" "one" myhash)
(puthash "2" "two" myhash)
(puthash "display" "1" myhash)
(puthash "val" "2" myhash)

;; (defun counsel-foo-fn (&rest args)
;;   '(("1" . "one") ("2" . "two")))

(defun counsel-foo-fn (&rest args)
  (list myhash myhash))
