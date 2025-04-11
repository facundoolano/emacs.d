(require 'facundo-programming)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :defer t
  :hook ((rustic-mode . subword-mode)
         (rustic-compilation-mode . visual-line-mode))
  :bind (:map rustic-mode-map
              ("s-r" . rustic-compile)
              ("s-R" . rustic-cargo-release)
              ("s-f" . rustic-cargo-fmt))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-compile-command "cargo clippy")
  (compilation-read-command nil)
  (rustic-compile-backtrace t))

(setq-default eglot-workspace-configuration
              (cons '(rust-analyzer . (:checkOnSave (:command "clippy")))
                    eglot-workspace-configuration))

(defun rustic-cargo-release ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo build --release"))

(provide 'facundo-rust)
