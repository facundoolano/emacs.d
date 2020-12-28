;;; facundo-ui.el --- Emacs Prelude: UI optimizations and tweaks.

;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (zenburn).

;;; Code:

;;; TAKEN FROM prelude-ui.el

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; FIXME update this to also show project name if any
;; and then drop projectile-nameframe

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; (setq frame-title-format
;;       '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
;;                                             (abbreviate-file-name (buffer-file-name))
;;                                           "%b"))))

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

;;; CUSTOM STUFF

(prelude-require-packages '(monokai-theme hl-todo github-modern-theme))

;;; Sublime like color theme
;; (load-theme 'monokai t)
(load-theme 'leuven t)
;; (load-theme 'github-modern t)
;; (load-theme 'smart-mode-line-light t)
(setq-default line-spacing 8)

;;; show line numbers, but not on neotree
;; (setq linum-format 'dynamic)
(setq linum-format "%3d ")
;; (add-hook 'prog-mode-hook 'linum-mode)
(global-set-key (kbd "<f7>") 'linum-mode)

(set-face-background 'vertical-border "white")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

;; set the fringe to work as a small margin of the same color as the bg
(fringe-mode '(10 . 0))
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; highlights todo and fixme
(require 'hl-todo)
(global-hl-todo-mode t)
(setq hl-todo-activate-in-modes '(prog-mode emacs-lisp-mode))

(provide 'facundo-ui)
;;; prelude-ui.el ends here
