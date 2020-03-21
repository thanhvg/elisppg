;; (spacemacs|define-custom-layout "@eww"
;;   :binding "w"
;;   :body
;;   (progn
;;     (defun spacemacs-layouts/add-eww-buffer-to-persp ()
;;       (persp-add-buffer (current-buffer)
;;                         (persp-get-by-name "@eww")))
;;     (add-hook 'eww-mode-hook #'spacemacs-layouts/add-eww-buffer-to-persp)
;;     (eww)))

(defvar thanh/net-buffers '("*HN*" "*eww*" "*How Do You*"))

(defun thanh/find-buffer (buffers)
  (if-let (buf (seq-find #'get-buffer buffers))
      buf
    (get-buffer-create (car buffers))))

(spacemacs|define-custom-layout "@www"
  :binding "w"
  :body
  (switch-to-buffer (thanh/find-buffer thanh/net-buffers)))
  
