(require 'facundo-programming)
(prelude-require-packages '(rustic lsp-mode))

(require 'rustic)
(require 'lsp)

(add-hook 'rustic-mode-hook #'subword-mode)

;; (customize-set-variable 'racer-complete-insert-argument-placeholders nil)

;; uncomment for less flashiness
;; (setq lsp-eldoc-hook nil)
;; (setq lsp-enable-symbol-highlighting nil)
;; (setq lsp-signature-auto-activate nil)

(push 'rustic-clippy flycheck-checkers)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-enable-snippet nil)
(setq rustic-format-on-save nil)
(setq rustic-format-trigger 'on-compile)
(setq rustic-compile-command "cargo clippy")
(setq compilation-read-command nil)

(define-key rustic-mode-map (kbd "M-?") 'lsp-describe-thing-at-point)
(define-key rustic-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key rustic-mode-map (kbd "M-,") 'pop-tag-mark)
(define-key rustic-mode-map (kbd "s-r") 'rustic-compile)

;; (setq lsp-enable-hover nil)
(setq lsp-signature-auto-activate nil)

(provide 'facundo-rust)
