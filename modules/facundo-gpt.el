(defun facundo/gptel-buffer ()
  "Switch or create a gptel session buffer, sending the marked region (if any)
to the prompt. Similar to setting the `g` flag in the gptel menu."
  (interactive)
  (gptel (concat "**GPTel**") nil  nil t)
  (with-current-buffer (other-buffer nil t)))

(defcustom facundo/gptel-save-directory nil
  "Directory to save gptel buffers to when using `facundo/gptel-buffer`
 with prefix. 
If nil, use regular `save-buffer` behavior with no default directory.
Directory will be created if it doesn't exist."
  :type '(choice (const :tag "Default behavior" nil) directory)
  :group 'gptel)

(defun facundo/gptel-buffer2 (&optional save-first)
  "Switch or create a gptel session buffer, sending the marked region (if any)
to the prompt. Similar to setting the `g` flag in the gptel menu.

With prefix argument (C-u), saves existing GPTel buffer first. If
`facundo/gptel-save-directory` is set, saves to that directory."
  (interactive "P")
  (when (and save-first
             (get-buffer "**GPTel**"))
    (with-current-buffer "**GPTel**"
      (let ((default-directory
             (if facundo/gptel-save-directory
                 (progn
                   (unless (file-exists-p facundo/gptel-save-directory)
                     (make-directory facundo/gptel-save-directory t))
                   facundo/gptel-save-directory)
               default-directory)))
        (call-interactively 'save-buffer))))
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
  (("C-c g" . facundo/gptel-buffer2)
   :map gptel-mode-map ("C-c RET" . facundo/gptel-send))
  :hook (gptel-post-response-functions . gptel-end-of-response))


(provide 'facundo-gpt)
