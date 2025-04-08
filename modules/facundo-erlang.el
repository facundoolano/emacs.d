;;; facundo-erlang.el --- Emacs Prelude: Erlang programming support.
;;
;;; Commentary:

;; Erlang is a concurrent functional language.

;;; Code:

(require 'facundo-programming)

;; FIXME removed all the erlang-ivy-complete stuff during my use-package migratino
;; should probably look into a modern lsp setup
;; or check git history and restore and update if that doesn't work

(use-package erlang
  :hook ((erlang-mode . subword-mode)))

;; default to erlang mode in .config files
;; (add-to-list 'prelude-indent-sensitive-modes 'erlang-mode)
;; (add-to-list 'auto-mode-alist '("\\.config\\’" . erlang-mode))
(add-to-list 'sp-no-reindent-after-kill-modes 'erlang-mode)
(add-to-list 'erlang-electric-commands 'erlang-electric-newline)

(setq erlang-indent-level 4)

;; add a smart pair for binaries
;; TODO add smarter sp configuration for these
(sp-local-pair 'erlang-mode "<<\"" "\">>")
(sp-local-pair 'erlang-mode "#{" "}")

(provide 'facundo-erlang)

;;; facundo-erlang.el ends here
