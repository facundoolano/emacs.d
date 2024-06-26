(require 'facundo-programming)
(prelude-require-packages '(rustic lsp-mode))

(require 'rustic)
(require 'lsp)

(add-hook 'rustic-mode-hook #'subword-mode)
(add-hook 'rustic-compilation-mode-hook 'visual-line-mode)

;; to update lsp and rustic, delete and install instead of reinstalling
;; https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/#updating-packageel-packages

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

;; making a shortcut since I cannot consistently turn it on-off on startup
(defun rust-toggle-hints ()
  "Toggle inlay hints mode"
  (interactive)
  (if lsp-rust-analyzer-inlay-hints-mode
      (lsp-rust-analyzer-inlay-hints-mode -1)
    (lsp-rust-analyzer-inlay-hints-mode 1)))

(define-key rustic-mode-map (kbd "s-r") 'rustic-compile)
(define-key rustic-mode-map (kbd "s-R") 'rustic-cargo-release)
(define-key rustic-mode-map (kbd "s-f") 'rustic-cargo-fmt)
(define-key rustic-mode-map (kbd "M-h") 'rust-toggle-hints)

;; (setq lsp-enable-hover nil)
(setq lsp-signature-auto-activate nil)

(provide 'facundo-rust)
