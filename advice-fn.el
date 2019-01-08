(defun his-tracing-function (orig-fun &rest args)
  (message "display-buffer called with args %S" args)
  (let ((res (apply orig-fun args)))
    (message "display-buffer returned %S" res)
    res))

;; (advice-add 'display-buffer :around #'his-tracing-function)
(advice-add 'neo-open-file :around #'his-tracing-function)

(advice-remove 'neo-open-file #'his-tracing-function)


(advice-remove 'neo-open-file #'user-neo-wrapper-function)
(defun user-neo-wrapper-function (orig-fun &rest args)
  (message "fn called with args %S" args)
  (let* ((added-args (if (second args) args
                        (list (first args)
                                (winum-get-number (get-mru-window)))))
        (res (apply orig-fun added-args)))
    (message "fn called %S returned %S" added-args res)
    res))

(advice-add 'neo-open-file :around #'user-neo-wrapper-function)

