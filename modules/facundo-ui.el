;;; facundo-ui.el --- Emacs Prelude: UI optimizations and tweaks.

;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (zenburn).

;;; Code:

(use-package diff-hl)
(use-package hl-todo)
(use-package which-key)

(require 'diff-hl-margin)
(require 'project)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" (:eval
            (cond
             ((project-current)
              (abbreviate-file-name (project-root (project-current))))
             ((buffer-file-name)
              (abbreviate-file-name (buffer-file-name)))
             (t "%b")))))

;; show available keybindings after you start typing
(which-key-mode +1)

(setq-default line-spacing 8)

;; setup side margins
(setq-default left-margin-width 1)
(setq-default right-margin-width 0)

(defun set-extra-margin ()
  "Add an extra bit of margin for text editing buffers."
  (setq left-margin-width 2))

;; (add-hook 'text-mode-hook 'set-extra-margin)
(add-hook 'prog-mode-hook 'set-extra-margin)

(set-face-background 'vertical-border "light gray")
(set-face-foreground 'vertical-border (face-background 'vertical-border))
(fringe-mode '(0 . 0))

;; hide the icon in the title bar
(setq ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))

;;; show line numbers on demand
(global-set-key (kbd "<f7>") 'display-line-numbers-mode)
(set-face-attribute 'line-number nil
                    :background (face-background 'default)
                    :foreground "gray")


;; diff-hl
;; NOTE diff-hl overrides the width var when toggling
;; I vendored the relevant file and removed that override
(customize-set-variable 'diff-hl-margin-symbols-alist
                        '((insert . " ")
                          (delete . " ")
                          (change . " ")
                          (unknown . " ")
                          (ignored . " ")))

(global-diff-hl-mode +1)
(diff-hl-margin-mode)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(global-hl-todo-mode t)
(setq hl-todo-activate-in-modes '(prog-mode emacs-lisp-mode))

(provide 'facundo-ui)
;;; prelude-ui.el ends here
