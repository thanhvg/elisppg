(defun gptel-add-code-doc (bounds &optional directive)
  "Add documentation to a code block using GPTEL.
BOUNDS is a cons of (BEGIN . END) positions specifying the code block.
If BOUNDS is nil or the region is active, uses the region's boundaries.
If BOUNDS is nil and the region is not active, uses the current defun.
Optional DIRECTIVE is a string to further instruct the model."
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     (t (cons (save-excursion (beginning-of-defun) (point))
              (save-excursion (end-of-defun) (point)))))
    (and current-prefix-arg
         (read-string "Directive: "
                      "You are a programmer."))))

  (let ((code-to-update (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (kill-region (car bounds) (cdr bounds))
    (gptel-request
       (format "Add the documentation for the following %s %s function(s), method(s) or class.
Respect the original indentation style. Do not change any indetation or code. Only add doc don't change code.
Respond the code itself do not surround it with markdown format."
               (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode))
               code-to-update)
     :system (or directive "You are a programmer.")
     :buffer (current-buffer)
     :stream gptel-stream
     :callback
     (lambda (response info)
       (if (not response)
           (message "ChatGPT response failed with: %s" (plist-get info :status))
         (let* ((buf (plist-get info :buffer)))
           (with-current-buffer buf
             (when (stringp response) 
               (insert response)))))))))

(defun gptel-add-code-doc-2 (bounds &optional directive)
  "Add documentation to the code within BOUNDS using GPT-el.

BOUNDS is a cons cell of (START . END) indicating the region
to be documented.  If the region is active, the region's
boundaries are used.  Otherwise, the current defun is used.

Optional DIRECTIVE is a string providing specific instructions
to GPT-el about how to generate the documentation.  If not
provided, a default directive is used that instructs GPT-el to
add documentation in the style of the current major mode.

The code within BOUNDS is replaced with the documented version
returned by GPT-el.  The process involves sending a request to
GPT-el, and upon receiving a response, inserting the documented
code into the buffer."
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     (t (cons (save-excursion (beginning-of-defun) (point))
              (save-excursion (end-of-defun) (point)))))
    (and current-prefix-arg
         (read-string "Directive: "
                      "You are a programmer."))))

  (let ((code-to-update (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (kill-region (car bounds) (cdr bounds))
    (gptel-request code-to-update
      :system (or directive
                  (format "You are a programmer.
Update the prompt with the documentation according to the style of %s.
Use the following guidelines:
- Respect the original indentation style. Do not change any indetation or code.
- Only add documentation don't change code.
- Respond concisely ONLY the code itself, no explanation or summary,
- Do not surround it with markdown format."
                          (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode))))
      :buffer (current-buffer)
      :stream gptel-stream
      :callback
      (lambda (response info)
        (if (not response)
            (message "Response failed with: %s" (plist-get info :status))
          (let* ((buf (plist-get info :buffer)))
            (with-current-buffer buf
              (when (stringp response) 
                (insert response)))))))))
