(require 'facundo-programming)
(prelude-require-packages '(rust-mode racer))

(setq rust-format-on-save t)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(customize-set-variable 'racer-complete-insert-argument-placeholders nil)

(defun racer-describe-and-switch ()
  "`racer-describe` symbol at point and switch to the temp buffer for easier killing."
  (interactive)
  (racer-describe)
  (other-window 1))

(define-key rust-mode-map (kbd "M-?") 'racer-describe-and-switch)

(provide 'facundo-rust)
