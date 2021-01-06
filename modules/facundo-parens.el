;;; facundo-parens.el --- parinfer/lisp edition configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;;; Commentary:

;;

;;; Code:


;;; TAKEN FROM prelude-editor.el

;; smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(setq sp-escape-quotes-after-insert nil)

(show-smartparens-global-mode +1)

;; don't want sp to mess with these global bindings
(define-key smartparens-mode-map (kbd "<M-up>") nil)
(define-key smartparens-mode-map (kbd "<M-down>") nil)


(defun prelude-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
;; FIXME: pick terminal friendly binding
;; (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
(define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;;; CUSTOM STUFF

(prelude-require-package 'parinfer)
(require 'parinfer)

;; use regular yank to avoid weird region replacement
(define-key parinfer-region-mode-map [remap yank] 'yank)
;; (define-key parinfer-region-mode-map (kbd "<tab>") 'parinfer-shift-right)
;; (define-key parinfer-region-mode-map (kbd "<backtab>") 'parinfer-shift-left)

(define-key parinfer-mode-map (kbd "<tab>") 'parinfer-smart-tab:dwim-right-or-complete)
(define-key parinfer-mode-map (kbd "<backtab>") 'parinfer-smart-tab:dwim-left)

;; enable some paredit commands
(define-key parinfer-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key parinfer-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key parinfer-mode-map (kbd "C-{") 'sp-forward-barf-sexp)
(define-key parinfer-mode-map (kbd "C-}") 'sp-backward-barf-sexp)

;; Redefine defaults to avoid unwanted extensions
(setq parinfer-extensions '(defaults pretty-parens))
(add-hook 'parinfer-mode-enable-hook #'parinfer--switch-to-paren-mode)

(global-set-key (kbd "s-(") 'parinfer-toggle-mode)

(provide 'facundo-parens)
;;; facundo-parens.el ends here
