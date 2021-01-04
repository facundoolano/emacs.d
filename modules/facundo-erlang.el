;;; facundo-erlang.el --- Emacs Prelude: Erlang programming support.
;;
;;; Commentary:

;; Erlang is a concurrent functional language.

;;; Code:

;;; TAKEN FROM prelude-erlang.el

(require 'facundo-programming)
(prelude-require-packages '(erlang ivy-erlang-complete company-erlang))

;;; CUSTOM STUFF

;; default to erlang mode in .config files
(add-to-list 'prelude-indent-sensitive-modes 'erlang-mode)
(add-to-list 'auto-mode-alist '("\\.config\\’" . erlang-mode))
(add-to-list 'sp-no-reindent-after-kill-modes 'erlang-mode)

(setq erlang-root-dir "/Users/facundo/kerl/21.3")
(setq exec-path (cons "/Users/facundo/kerl/21.3/bin" exec-path))

;; never managed to get the man working
(setq erlang-man-root-dir "/usr/local/lib/erlang/man")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.9.1/emacs" load-path))


(setq erlang-indent-level 4)

;; rebar3 checker trips with rebar2 projects
;; (setq-default flycheck-disabled-checkers '(erlang-rebar3))

;; add a smart pair for binaries
(sp-local-pair 'erlang-mode "<<\"" "\">>")
(sp-local-pair 'erlang-mode "#{" "}")

;; NOTE I had to replace sed with gsed in ivy-erlang-complete exported funcs for them to work on macos
(setq ivy-erlang-complete-erlang-root "/Users/facundo/kerl/21.3")
(add-hook 'erlang-mode-hook #'ivy-erlang-complete-init)
;; automatic update completion data after save
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)
(add-hook 'erlang-mode-hook #'company-erlang-init)

(provide 'facundo-erlang)

;;; facundo-erlang.el ends here
