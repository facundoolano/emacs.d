;; taken from https://emacsredux.com/blog/2025/03/28/speed-up-emacs-startup-by-tweaking-the-gc-settings/

;; (setq use-package-compute-statistics t)

(setq large-file-warning-threshold 100000000)

;; Temporarily increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum)


;; Restore to normal value after startup (e.g. 100MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 100 1024 1024))))

;; Disable toolbars, menus, and other visual elements for faster startup:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'leuven t)

(add-to-list 'default-frame-alist '(font . "Menlo-14"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
