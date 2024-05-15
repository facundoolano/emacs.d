(require 'facundo-programming)

(prelude-require-packages '(lsp-mode))

(setq lsp-solargraph-use-bundler nil)

(add-hook 'ruby-mode-hook (lambda ()
                            (lsp)))


(provide 'facundo-ruby)
