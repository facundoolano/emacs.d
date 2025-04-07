(require 'facundo-programming)

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . subword-mode)
         (go-mode . (lambda () (setq tab-width 4)))))

(provide 'facundo-go)
