(defun facundo/gptel-buffer ()
  "Switch or create a gptel session buffer, sending the marked region (if any)
to the prompt. Similar to setting the `g` flag in the gptel menu."
  (interactive)
  (gptel (concat "**GPTel**") nil  nil t)
  (with-current-buffer (other-buffer nil t)))

(defun facundo/gptel-send ()
  "send the last prompt, making sure not in a middle of a sentence"
  (interactive)
  (when (and (not (use-region-p))
             (not (eolp)))
    (move-end-of-line 1))
  (gptel-send))

(use-package gptel
  :custom 
  (gptel-log-level 'info)
  (gptel-default-mode 'org-mode)
  (gptel-backend (gptel-get-backend "Claude"))
  (gptel-model 'claude-3-7-sonnet-20250219)
  :bind
  (("C-c g" . facundo/gptel-buffer)
   :map gptel-mode-map ("C-c RET" . facundo/gptel-send))
  :hook (gptel-post-response-functions . gptel-end-of-response)
  :config
  (gptel-make-anthropic "Claude"
    :key gptel-api-key ;; get it from ~/.authinfo file
    :stream t))

(provide 'facundo-gpt)
