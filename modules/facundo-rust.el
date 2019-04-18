(require 'facundo-programming)
(prelude-require-packages '(rust-mode racer))

(require 'rust-mode)

(setq rust-format-on-save t)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook (lambda ()
                            ;; rust-clippy only outputs warnings on the first run after a change. disabling other checks so warnings are not lost
                            ;; https://github.com/rust-lang/rust-clippy/issues/2604
                            (setq flycheck-check-syntax-automatically '(mode-enabled save))
                            (setq flycheck-checker 'rust-clippy)))
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(customize-set-variable 'racer-complete-insert-argument-placeholders nil)

(defun racer-describe-and-switch ()
  "`racer-describe` symbol at point and switch to the temp buffer for easier killing."
  (interactive)
  (racer-describe)
  (other-window 1))

(define-key rust-mode-map (kbd "M-?") 'racer-describe-and-switch)
(define-key rust-mode-map (kbd "s-r") 'rust-run-clippy) ;; same as build but with lint warnings

(provide 'facundo-rust)
