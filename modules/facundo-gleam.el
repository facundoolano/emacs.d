(use-package gleam-ts-mode
  :mode ("\\.gleam\\'" . gleam-ts-mode)
  :requires lsp-mode
  :hook (gleam-ts-mode . lsp-deferred))

(provide 'facundo-gleam)
