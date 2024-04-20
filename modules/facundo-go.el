(require 'facundo-programming)

(prelude-require-packages '(go-mode lsp-mode))

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)
                          (flycheck-add-next-checker 'lsp 'go-vet)
                          (flycheck-add-next-checker 'lsp 'go-staticcheck)))


(provide 'facundo-go)
