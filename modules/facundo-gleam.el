(prelude-require-packages '(gleam-ts-mode))

(require 'lsp-mode)
(require 'gleam-ts-mode)

(add-hook 'gleam-ts-mode-hook 'lsp-deferred)

(provide 'facundo-gleam)
