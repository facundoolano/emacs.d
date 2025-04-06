;;; facundo-programming.el --- Emacs Prelude: prog-mode configuration
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;;; Commentary:

;; Some basic prog-mode configuration and programming related utilities.

;;; Code:

(use-package smartparens)
(use-package flycheck)
(use-package lsp-mode)
(use-package apheleia)

;;; TAKEN FROM prelude-programming.el

;; smart curly braces
;; (sp-pair "{ nil :post-handlers
;;          '(((lambda (&rest _ignored)
;;               (crux-smart-open-line-above)) "RET")))

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prelude-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  (electric-pair-mode -1)
  (smartparens-mode +1)
  (prelude-enable-whitespace)
  (prelude-local-comment-auto-fill))

(add-hook 'prog-mode-hook 'prelude-prog-mode-defaults)
(add-hook 'prog-mode-hook (lambda () (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))))

;; format on save with apheleia
;; override python defaults
;; should also pick up gofmt and prettier without config
(apheleia-global-mode)
(setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
(setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

;;; CUSTOM STUFF

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun insert-todo ()
  "Add a TODO comment in the line above."
  (interactive)
  (crux-smart-open-line-above)
  (insert "TODO ")
  (comment-or-uncomment-region-or-line))

(defun insert-fixme ()
  "Add a FIXME comment in the line above."
  (interactive)
  (crux-smart-open-line-above)
  (insert "FIXME ")
  (comment-or-uncomment-region-or-line))

(defun lsp-find-definition-other-window ()
  "Find definiton in other window."
  (interactive)
  (lsp-find-definition :display-action 'window))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-;") 'insert-todo)
(global-set-key (kbd "s-:") 'insert-fixme)

;; sexp commands tend to work well in non lisp langs too, so binding them globally
(global-set-key (kbd "C-M-f") 'forward-sexp)
(global-set-key (kbd "C-M-b") 'backward-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-s") 'sp-splice-sexp)

;; FIXME move to a lsp module
(define-key lsp-mode-map (kbd "M-?") 'lsp-describe-thing-at-point)
(define-key lsp-mode-map (kbd "M-?") 'lsp-describe-thing-at-point)
(define-key lsp-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key lsp-mode-map (kbd "C-M-.") 'lsp-find-definition-other-window)
(define-key lsp-mode-map (kbd "M-,") 'pop-tag-mark)

;; lsp makes too many file watches and trips on big projects.
;; this is supposed to bring it back to life
;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
;; see lsp-file-watch-ignored-directories and lsp-file-watch-ignored-files
;; https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))


(setq lsp-file-watch-threshold 5000)
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.node_modules\\'")
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.asdf\\'")

(provide 'facundo-programming)
;;; facundo-programming.el ends here
