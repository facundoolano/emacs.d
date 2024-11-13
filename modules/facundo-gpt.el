(prelude-require-packages '(gptel))
(require 'gptel)

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(gptel-make-anthropic "Claude"
  :key gptel-api-key ;; get it from ~/.authinfo file
  :stream t)

(setq gptel-model "gpt-4o")
(setq gptel-default-mode 'org-mode)

(defun facundo/gptel-buffer ()
  "Switch or create a gptel session buffer, sending the marked region (if any)
to the prompt. Similar to setting the `g` flag in the gptel menu."
  (interactive)
  (gptel (concat "**GPTel**") nil  nil t)
  (with-current-buffer (other-buffer nil t)))

(defun facundo/gptel-send ()
  "TODO"
  (interactive)
  (when (and (not (use-region-p))
             (not (eolp)))
    (move-end-of-line 1))
  (gptel-send))

(global-set-key (kbd "C-c g") 'facundo/gptel-buffer)
(define-key gptel-mode-map (kbd "C-c RET") 'facundo/gptel-send)



;; TODO
;; - option to use claude alternatively

(provide 'facundo-gpt)
