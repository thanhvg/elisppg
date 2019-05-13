;; indent motion - begin
;; https://emacs.stackexchange.com/questions/20900/navigate-by-indentation
(defun indentation-get-next-good-line (direction skip good)
  "Moving in direction `direction', and skipping over blank
lines and lines that satisfy relation `skip' between their
indentation and the original indentation, finds the first line
whose indentation satisfies predicate `good'."
  (let ((starting-indentation (current-indentation))
        (lines-moved direction))
    (save-excursion
      (while (and (zerop (forward-line direction))
                  (or (eolp)  ; Skip past blank lines and other skip lines
                      (funcall skip (current-indentation) starting-indentation)))
        (setq lines-moved (+ lines-moved direction)))
      ;; Now we can't go further. Which case is it?
      (if (and
           (not (eobp))
           (not (bobp))
           (funcall good (current-indentation) starting-indentation))
          lines-moved
        nil))))

(defun indentation-get-next-sibling-line ()
  "The line number of the next sibling, if any."
  (indentation-get-next-good-line 1 '> '=))

(defun indentation-get-previous-sibling-line ()
  "The line number of the previous sibling, if any"
  (indentation-get-next-good-line -1 '> '=))

(defun indentation-get-parent-line ()
  "The line number of the parent, if any."
  (indentation-get-next-good-line -1 '>= '<))

(defun indentation-get-uncle-line ()
  "The line number of the parent, if any."
  (indentation-get-next-good-line 1 '>= '<))

(defun indentation-get-child-line ()
  "The line number of the first child, if any."
  (indentation-get-next-good-line +1 'ignore '>))

(defun indentation-get-child-or-newphew-line ()
  "The line number of the first child, if any."
  (indentation-get-next-good-line +1 '= '>))

(defun indentation-move-to-line (func preserve-column name)
  "Move the number of lines given by func. If not possible, use `name' to say so."
  (let ((saved-column (current-column))
        (lines-to-move-by (funcall func)))
    (if lines-to-move-by
        (progn
          (forward-line lines-to-move-by)
          (move-to-column (if preserve-column
                              saved-column
                            (current-indentation))))
      (message "No %s to move to." name))))

(defun indentation-forward-to-next-sibling ()
  "Move to the next sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-next-sibling-line t "next sibling"))

(defun indentation-backward-to-previous-sibling ()
  "Move to the previous sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-previous-sibling-line t "previous sibling"))

(defun indentation-up-to-parent ()
  "Move to the parent line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-parent-line nil "parent"))

(defun indentation-forward-to-next-uncle ()
  "Move to the next uncle line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-uncle-line nil "uncle"))

(defun indentation-down-to-child ()
  "Move to the first child line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-child-line nil "child"))

(defun indentation-down-to-child-or-nephew ()
  "Move to the first child line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-child-or-newphew-line nil "child"))

;; evil motion wrapper
(evil-define-motion evil-indentation-forward-to-next-uncle (count)
  :jump t
  :type exclusive
  (let ((c (or count 1)))
    (dotimes (i c) (indentation-forward-to-next-uncle))))

(evil-define-motion evil-indentation-up-to-parent (count)
  :jump t
  :type exclusive
  (let ((c (or count 1)))
    (dotimes (i c) (indentation-up-to-parent))))

(evil-define-motion evil-indentation-forward-to-next-sibling (count)
  :jump t
  :type exclusive
  (let ((c (or count 1)))
    (dotimes (i c) (indentation-forward-to-next-sibling))))

(evil-define-motion evil-indentation-backward-to-previous-sibling (count)
  :jump t
  :type exclusive
  (let ((c (or count 1)))
    (dotimes (i c) (indentation-backward-to-previous-sibling))))

(evil-define-motion evil-indentation-down-to-child-or-nephew (count)
  :jump t
  :type exclusive
  (let ((c (or count 1)))
    (dotimes (i c) (indentation-down-to-child-or-nephew))))

;; evil define key
(define-key evil-motion-state-map "]a" 'evil-indentation-forward-to-next-uncle)
(define-key evil-motion-state-map "[a" 'evil-indentation-up-to-parent)
(define-key evil-motion-state-map "]z" 'evil-indentation-forward-to-next-sibling)
(define-key evil-motion-state-map "[z" 'evil-indentation-backward-to-previous-sibling)
(define-key evil-motion-state-map "]c" 'evil-indentation-down-to-child-or-nephew)
;; indent motion - end
