;;; facundo-org.el --- Emacs Prelude: org-mode configuration.
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; Code:

(prelude-require-package 'org)

(require 'org)
(require 'org-present)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(defun prelude-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))

(add-hook 'org-mode-hook 'prelude-org-mode-defaults)

(setq org-startup-folded nil)

;; allow to resize images
(setq org-image-actual-width nil)

;; syntax highlight code snippets
(setq org-src-fontify-natively t)

;; hiding these specifically because I want bold table headings in org-present
(setq org-hide-emphasis-markers t)

;; setup presentation mode
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (toggle-frame-fullscreen)
                 (org-display-inline-images)
                 (whitespace-mode -1)
                 (flyspell-mode -1)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (toggle-frame-fullscreen)
                 (org-remove-inline-images)
                 (whitespace-mode +1)
                 (flyspell-mode +1)
                 (org-present-read-write)))))

(define-key org-mode-map (kbd "<f5>") 'org-present)
(define-key org-present-mode-keymap (kbd "q") 'org-present-quit)
(define-key org-present-mode-keymap (kbd "<f5>") 'org-present-quit)

(provide 'facundo-org)

;;; facundo-org.el ends here
