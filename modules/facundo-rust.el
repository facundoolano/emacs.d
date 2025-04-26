(require 'facundo-programming)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . subword-mode)
         (rustic-compilation-mode . visual-line-mode))
  :bind (:map rustic-mode-map
              ("s-r" . rustic-compile)
              ("s-R" . rustic-cargo-release)
              ("M-h" . lsp-inlay-hints-mode)
              ("s-f" . rustic-cargo-fmt))
  :custom
  (lsp-inlay-hint-enable t)
  (rustic-compile-command "cargo clippy")
  (compilation-read-command nil)
  (rustic-compile-backtrace t))

(push 'rustic-clippy flycheck-checkers)

(defun rustic-cargo-release ()
  "Run 'cargo build' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo build --release"))

(provide 'facundo-rust)
