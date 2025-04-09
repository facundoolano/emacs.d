(require 'facundo-programming)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :defer t
  :hook ((rustic-mode . subword-mode)
         (rustic-compilation-mode . visual-line-mode))
  :bind (:map rustic-mode-map ("s-r" . rustic-compile)
              ("s-R" . rustic-cargo-release)
              ("s-f" . rustic-cargo-fmt)
              ("M-h" . rust-toggle-hints)))

(setq rustic-lsp-client 'eglot)
(setq-default eglot-workspace-configuration
              '((:rust-analyzer . (:checkOnSave (:command "clippy")))))

(setq rustic-compile-command "cargo clippy")
(setq compilation-read-command nil)

;; show backtace on test output
(setq rustic-compile-backtrace t)

(defun rustic-cargo-release ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo build --release"))

(provide 'facundo-rust)
