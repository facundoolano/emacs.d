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
(add-to-list 'prelude-indent-sensitive-modes 'erlang-mode)
(add-to-list 'auto-mode-alist '("\\.config\\’" . erlang-mode))
(add-to-list 'sp-no-reindent-after-kill-modes 'erlang-mode)

(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

;; never managed to get the man working
(setq erlang-man-root-dir "/usr/local/lib/erlang/man")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.9.1/emacs" load-path))

(setq erlang-indent-level 4)
;; (setq flycheck-erlang-include-path (list "../" "../include/" "../../include/"))

;; rebar3 checker trips with rebar2 projects
(setq-default flycheck-disabled-checkers '(erlang-rebar3))

;; add a smart pair for binaries
(sp-local-pair 'erlang-mode "<<\"" "\">>")
(sp-local-pair 'erlang-mode "#{" "}")

(defun my-erlang-mode-hook ()
  (local-set-key (kbd "s-j") 'erlang-shell))

(defun my-erlang-shell-mode-hook ()
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

;; Some Erlang customization
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(add-hook 'erlang-shell-mode-hook 'my-erlang-shell-mode-hook)

(defun counsel-erlplorer-function (string)
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (format "erlplorer search \"%s\" %ssrc/*.erl"
             string
             (projectile-project-root)))
    nil))

(defun counsel-erlplorer-action (string)
  (when string
    (let* ((location-parts (split-string (first (split-string string)) ":"))
           (filename (first location-parts))
           (line-number (string-to-number (second location-parts))))
      (with-ivy-window
        (find-file filename)
        (forward-line (1- line-number))
        (swiper--ensure-visible)))))

(defun counsel-erlplorer ()
  "Call the \"erlplorer\" shell command to grep erlang patterns.
See http://marianoguerra.org/posts/erplorer-search-erlang-and-efene-code-by-pattern-matching-the-ast.html"
  (interactive)
  (ivy-read "Search Erlang pattern: " #'counsel-erlplorer-function
            :dynamic-collection t
            :history 'counsel-erlplorer-history
            :action #'counsel-erlplorer-action
            :unwind #'counsel-delete-process
            :caller 'counsel-erlplorer))

(provide 'facundo-erlang)

;;; facundo-erlang.el ends here
