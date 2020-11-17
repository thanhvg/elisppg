(defun thanh-company ()
  (interactive)
  (let ((company-backends '(company-dabbrev)))
    (company-complete)))
