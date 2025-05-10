(defcustom facundo/gptel-save-directory nil
  "Directory to save gptel buffers to when using `facundo/gptel-buffer`
 with prefix. 
If nil, use regular `save-buffer` behavior with no default directory.
Directory will be created if it doesn't exist."
  :type '(choice (const :tag "Default behavior" nil) directory)
  :group 'gptel)

(defun facundo/gptel-generate-filename ()
  "Generate a default filename for GPTel buffer based on content.
Includes timestamp prefix and then the first prompt (after '***') if found."
  (with-current-buffer "**GPTel**"
    (let* ((timestamp (format-time-string "%Y%m%d"))
           (first-prompt
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward "^\\*\\*\\* \\(.*\\)" nil t)
                  (match-string-no-properties 1)
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (min (+ (line-beginning-position) 40) (line-end-position))))))
           (sanitized-prompt
            (if (string-empty-p (string-trim first-prompt))
                ""
              (let ((name first-prompt))
                (setq name (replace-regexp-in-string "[^a-zA-Z0-9_-]+" "-" name))
                (setq name (replace-regexp-in-string "-+" "-" name))
                (setq name (replace-regexp-in-string "^-\\|-$" "" name))
                (setq name (substring name 0 (min (length name) 30)))
                name))))
      (if (string-empty-p sanitized-prompt)
          (concat timestamp ".org")
        (concat timestamp "-" sanitized-prompt ".org")))))

(defun facundo/gptel-buffer (&optional arg)
  "Switch or create a gptel session buffer, sending the marked region (if any)
to the prompt. Similar to setting the `g` flag in the gptel menu.

With prefix argument (C-u), saves existing GPTel buffer first. If
`facundo/gptel-save-directory` is set, saves to that directory."
  (interactive "P")
  (when (and arg (get-buffer "**GPTel**"))
    (let ((gptel-buffer (get-buffer "**GPTel**")))
      ;; Switch to the GPTel buffer first
      (switch-to-buffer-other-window gptel-buffer)
      ;; Configure directory and save
      (let* ((default-directory
              (if facundo/gptel-save-directory
                  (progn
                    (unless (file-exists-p facundo/gptel-save-directory)
                      (make-directory facundo/gptel-save-directory t))
                    facundo/gptel-save-directory)
                default-directory))
             (suggested-name (facundo/gptel-generate-filename)))
        (write-file (read-file-name "Save as: " default-directory nil nil suggested-name)))))
  (gptel "**GPTel**" nil nil t)
  (with-current-buffer (other-buffer nil t)))

(defun facundo/gptel-send ()
  "send the last prompt, making sure not in a middle of a sentence"
  (interactive)
  (when (and (not (use-region-p))
             (not (eolp)))
    (move-end-of-line 1))
  (gptel-send))

(use-package gptel
  :init
  (gptel-make-anthropic "Claude"
    :key gptel-api-key ;; get it from ~/.authinfo file
    :stream t)
  :config
  (customize-set-variable 'gptel-backend (gptel-get-backend "Claude"))
  :custom 
  (gptel-log-level 'info)
  (gptel-default-mode 'org-mode)
  (gptel-model 'claude-3-7-sonnet-20250219)
  (facundo/gptel-save-directory "~/dev/gptel/")
  :bind
  (("C-c g" . facundo/gptel-buffer)
   :map gptel-mode-map ("C-c RET" . facundo/gptel-send))
  :hook (gptel-post-response-functions . gptel-end-of-response))


(provide 'facundo-gpt)
