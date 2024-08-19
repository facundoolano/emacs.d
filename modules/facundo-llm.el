(prelude-require-packages '(gptel))
(require 'gptel)

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(gptel-make-anthropic "Claude"
  :key gptel-api-key ;; get it from ~/.authinfo file
  :stream t)

(setq gptel-model "gpt-4o")

(defun facundo/gptel-send ()
  "Switch or create a gptel session buffer, sending the marked region (if any)
to the prompt. Similar to setting the `g` flag in the gptel menu."
  (interactive)
  (let ((text (if (use-region-p)
                  (kill-new (buffer-substring-no-properties (region-beginning) (region-end)))
                "")))
    (gptel (concat "**GPTel**") nil  nil t)
    (with-current-buffer (other-buffer nil t)
      (if (not (string= text ""))
          (yank)))))

(global-set-key (kbd "C-c g") 'facundo/gptel-send)

;; TODO
;; - option to use claude alternatively

(provide 'facundo-llm)
