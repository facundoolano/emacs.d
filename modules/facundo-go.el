(require 'facundo-programming)

(prelude-require-packages '(go-mode lsp-mode))

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; TODO verify that go-vet and go-staticcheck are enabled. this doesnt work anymore:
;; (flycheck-add-next-checker 'go-mode 'go-vet)
;; (flycheck-add-next-checker 'go-mode 'go-staticcheck)
(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)))




(provide 'facundo-go)
