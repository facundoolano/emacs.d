;;; facundo-erlang.el --- Emacs Prelude: Erlang programming support.
;;
;;; Commentary:

;; Erlang is a concurrent functional language.

;;; Code:

;;; TAKEN FROM prelude-erlang.el

(prelude-require-packages '(erlang))

(require 'facundo-programming)
(require 'ivy-erlang-complete)
(require 'company-erlang)


;;; CUSTOM STUFF

;; default to erlang mode in .config files
(add-to-list 'prelude-indent-sensitive-modes 'erlang-mode)
(add-to-list 'auto-mode-alist '("\\.config\\’" . erlang-mode))
(add-to-list 'sp-no-reindent-after-kill-modes 'erlang-mode)

(setq erlang-root-dir "~/.asdf/installs/erlang/22.3.4.4/")
(setq exec-path (cons "~/.asdf/installs/erlang/22.3.4.4/bin/" exec-path))

;; never managed to get the man working
(setq erlang-man-root-dir "/usr/local/lib/erlang/man")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.9.1/emacs" load-path))


(setq erlang-indent-level 4)

;; rebar3 checker trips with rebar2 projects
(setq-default flycheck-disabled-checkers '(erlang-rebar3))

;; add a smart pair for binaries
;; TODO add smarter sp configuration for these
(sp-local-pair 'erlang-mode "<<\"" "\">>")
(sp-local-pair 'erlang-mode "#{" "}")

;; NOTE I had to replace sed with gsed in ivy-erlang-complete exported funcs for them to work on macos
(setq ivy-erlang-complete-erlang-root "/Users/facundo/.asdf/installs/erlang/22.3.4.4/")
(setq ivy-erlang-complete-enable-autosave nil)
(add-hook 'erlang-mode-hook #'ivy-erlang-complete-init)
(add-hook 'erlang-mode-hook #'subword-mode)
;; automatic update completion data after save
(add-hook 'after-save-hook #'ivy-erlang-complete-reparse)
(add-hook 'erlang-mode-hook #'company-erlang-init)

(provide 'facundo-erlang)

;;; facundo-erlang.el ends here
