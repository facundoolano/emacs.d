;;; facundo-erlang.el --- Emacs Prelude: Erlang programming support.
;;
;;; Commentary:

;; Erlang is a concurrent functional language.

;;; Code:

;;; TAKEN FROM prelude-erlang.el

(require 'facundo-programming)
(prelude-require-packages '(erlang))

(defcustom wrangler-path nil
  "The location of wrangler elisp directory."
  :group 'prelude-erlang
  :type 'string
  :safe 'stringp)

(require 'projectile)

(when (require 'erlang-start nil t)

  (when (not (null wrangler-path))
    (add-to-list 'load-path wrangler-path)
    (require 'wrangler)))

(add-hook 'erlang-mode-hook (lambda ()
                              (setq erlang-compile-function 'projectile-compile-project)))

;;; CUSTOM STUFF

;; default to erlang mode in .config files
(add-to-list 'auto-mode-alist '("\\.config\\’" . erlang-mode))

(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

;; never managed to get the man working
(setq erlang-man-root-dir "/usr/local/lib/erlang/man")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.9.1/emacs" load-path))

(setq erlang-indent-level 2)
(setq flycheck-erlang-include-path (list "../" "../include/" "../../include/"))

;; rebar3 checker trips with rebar2 projects
(setq-default flycheck-disabled-checkers '(erlang-rebar3))

;; add a smart pair for binaries
(sp-local-pair 'erlang-mode "<<\"" "\">>")

(defun my-erlang-mode-hook ()
  (local-set-key (kbd "s-j") 'erlang-shell))

(defun my-erlang-shell-mode-hook ()
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

;; Some Erlang customization
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(add-hook 'erlang-shell-mode-hook 'my-erlang-shell-mode-hook)

(provide 'facundo-erlang)

;;; facundo-erlang.el ends here
