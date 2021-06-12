(require 'facundo-programming)
(prelude-require-packages '(rustic lsp-mode))

(require 'rustic)
(require 'lsp)

(add-hook 'rustic-mode-hook #'subword-mode)
(add-hook 'rustic-compilation-mode-hook 'visual-line-mode)

;; (customize-set-variable 'racer-complete-insert-argument-placeholders nil)

;; uncomment for less flashiness
;; (setq lsp-eldoc-hook nil)
;; (setq lsp-enable-symbol-highlighting nil)
;; (setq lsp-signature-auto-activate nil)

(push 'rustic-clippy flycheck-checkers)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-enable-snippet nil)
(setq rustic-format-on-save nil)
(setq rustic-format-trigger nil)
(setq rustic-compile-command "cargo clippy")
(setq compilation-read-command nil)
(setq lsp-rust-analyzer-server-display-inlay-hints t)

;; show backtace on test output
(setq rustic-compile-backtrace t)

(defun rustic-cargo-release ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo build --release"))

(define-key rustic-mode-map (kbd "M-?") 'lsp-describe-thing-at-point)
(define-key rustic-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key rustic-mode-map (kbd "M-,") 'pop-tag-mark)
(define-key rustic-mode-map (kbd "s-r") 'rustic-compile)
(define-key rustic-mode-map (kbd "s-R") 'rustic-cargo-release)
(define-key rustic-mode-map (kbd "s-f") 'rustic-cargo-fmt)

;; (setq lsp-enable-hover nil)
(setq lsp-signature-auto-activate nil)

(provide 'facundo-rust)
