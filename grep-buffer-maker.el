;;; grep-buffer-maker.el --- Grep buffer maker  -*- lexical-binding: t; -*-

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-virtual-comment
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO

;;; Code:
(require 'project)
(require 'thingatpt)
(require 'grep)
(require 'simple)

;;; Code

(define-minor-mode grep-buffer-maker-mode
  "Minor mode in grep buffer content."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") 'grep-buffer-maker-toggle)
            map))

(defun grep-buffer-maker--send-region (begin end)
  "Send buffer content between BEGIN and END."
  (let ((result nil))
    (save-excursion
      (goto-char end)
      (while (> (point) begin)
        (let ((line-num (line-number-at-pos nil t))
              (line (thing-at-point 'line t)))
          (setq result (append (list (cons line-num line)) result)))
        (forward-line -1)))
    result))

(defun grep-buffer-maker--ensure-buffer (project)
  "Find the buffer and set up boilerplate for PROJECT."
  (let* ((buff-name (format "*gbm-%s*" project))
         (buff (get-buffer buff-name)))
    (unless buff
      (setq buff (generate-new-buffer buff-name))
      (with-current-buffer buff
        (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                        project))))
    (switch-to-buffer-other-window buff)))

(defun grep-buffer-maker--write-cands (sequence project filename)
  "Write candidate to buffer."
  (with-current-buffer (grep-buffer-maker--ensure-buffer project)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (newline)
    (mapc (lambda (it)
            (insert (format "./%s:%s:%s" filename (car it) (cdr it)) ))
          sequence)
    (grep-mode)
    (grep-buffer-maker-mode)
    (run-hooks 'grep-buffer-maker-mode-hook)))

;;;###autoload
(defun grep-buffer-maker-region (begin end)
  "Send buffer content between BEGIN and END."
  (interactive "r")
  (grep-buffer-maker--write-cands (grep-buffer-maker--send-region begin end)
                                  (cdr (project-current))
                                  (file-relative-name
                                   (file-truename (buffer-file-name)) (cdr (project-current)))))

;;;###autoload
(defun grep-buffer-maker-dwim ()
  "Send buffer content."
  (interactive)
  (when buffer-file-name
      (if (region-active-p)
       (grep-buffer-maker-region (region-beginning) (region-end))
     (grep-buffer-maker-region
      (save-excursion
        (beginning-of-line)
        (point))
      (save-excursion
        (end-of-line)
        (point))))))

(defun grep-buffer-maker-toggle ()
  "Toggle buffer read-only state."
  (interactive)
  (if buffer-read-only
      (progn
        (setq buffer-read-only nil)
        (message "Buffer can be changed."))
    (setq buffer-read-only t)
    (message "Buffer is set to read-only state.")))

(provide 'grep-buffer-maker)
;;; grep-buffer-maker.el ends here
