;;; facundo-text.el --- text edition configuration

(require 'livedown)

(prelude-require-packages '(centered-window visual-fill-column))
(global-set-key (kbd "<f9>") 'centered-window-mode)

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun text-hook ()
  (whitespace-mode -1)
  (visual-line-mode)
  ;; (visual-fill-column-mode)
  )

;; wrap lines in text modes
(add-hook 'text-mode-hook 'text-hook)
(add-hook 'org-mode-hook 'text-hook)
(add-hook 'markdown-mode-hook 'text-hook)

(provide 'facundo-text)

;;; facundo-text.el ends here
