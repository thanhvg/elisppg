(which-key-add-keymap-based-replacements evil-normal-state-map (kbd "g s") "evil-easy-motion")

(which-key-add-keymap-based-replacements global-map (kbd "g s") "evil-easy-motion")

(which-key-add-keymap-based-replacements global-map (kbd "C-c C-w") "Save stuff")

(which-key-add-keymap-based-replacements graphql-mode-map "g s" "evil-easy-motion")
(which-key-add-keymap-based-replacements spacemacs-default-map (kbd "SPC g s" ) "evil-easy-motion")

(which-key-add-keymap-based-replacements spacemacs-default-map (kbd "g s" ) "evil-easy-motion")

(which-key-add-keymap-based-replacements spacemacs-tide-mode-map (kbd "SPCmE" ) "Errors")

(which-key-add-keymap-based-replacements spacemacs-tide-mode-map ("SPC m E") "Errors")


(defun spacemacs/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-major-mode-leader-key' and
`dotspacemacs-major-mode-emacs-leader-key' for the minor-mode
MODE. MODE should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `spacemacs/set-leader-keys'."
  (let* ((map (intern (format "spacemacs-%s-map" mode))))
    (when (spacemacs//init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))

spacemacs-evil-mode-map

(defun spacemacs/declare-prefix-for-minor-mode (mode prefix name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let* ((map (intern (format "spacemacs-%s-map" mode)))
         (prefix-map (intern (format "%s-prefix" map)))
         (full-prefix (concat dotspacemacs-leader-key " " prefix))
         (full-prefix-emacs (concat dotspacemacs-emacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat dotspacemacs-major-mode-leader-key
                                    " " (substring prefix 1)))
         (major-mode-prefix-emacs
          (concat dotspacemacs-major-mode-emacs-leader-key
                  " " (substring prefix 1))))
    (when (spacemacs//init-leader-mode-map mode map t)
      (which-key-add-keymap-based-replacements prefix-map full-prefix-emacs name)
      (which-key-add-keymap-based-replacements prefix-map full-prefix name)
      (when (and is-major-mode-prefix dotspacemacs-major-mode-leader-key)
        (which-key-add-keymap-based-replacements prefix-map major-mode-prefix name))
      (when (and is-major-mode-prefix dotspacemacs-major-mode-emacs-leader-key)
        (which-key-add-keymap-based-replacements prefix-map major-mode-prefix-emacs name)))))
