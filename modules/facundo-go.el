(require 'facundo-programming)

(prelude-require-packages '(go-mode lsp-mode))

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'subword-mode)

(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)))
                          ;; (flycheck-add-next-checker 'go-mode 'go-vet)
                          ;; (flycheck-add-next-checker 'go-mode 'go-staticcheck)



(provide 'facundo-go)
