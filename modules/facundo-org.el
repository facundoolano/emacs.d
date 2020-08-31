;;; facundo-org.el --- Emacs Prelude: org-mode configuration.
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; Code:

(prelude-require-packages '(org ox-gfm))

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

;; insert headings after the content, not right before current one
(setq org-insert-heading-respect-content t)

;; don't show line overflow in present mode
(setq whitespace-global-modes '(not org-present-mode))

;; by default don't include a toc when exporting e.g. to markdown
(setq org-export-with-toc nil)

;; setup presentation mode
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (toggle-frame-fullscreen)
                 (org-display-inline-images)
                 (flyspell-mode -1)
                 (whitespace-mode -1)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (toggle-frame-fullscreen)
                 (org-remove-inline-images)
                 (flyspell-mode +1)
                 (whitespace-mode +1)
                 (org-present-read-write)))))

(define-key org-mode-map (kbd "M-{") 'org-previous-visible-heading)
(define-key org-mode-map (kbd "M-}") 'org-next-visible-heading)
(define-key org-mode-map (kbd "M-p") 'org-metaup)
(define-key org-mode-map (kbd "M-n") 'org-metadown)
(define-key org-mode-map (kbd "M-<up>") 'move-text-up)
(define-key org-mode-map (kbd "M-<down>") 'move-text-down)

;; pasting this here as a cheatsheet
;; https://orgmode.org/manual/Structure-Templates.html
;; a	‘#+BEGIN_EXPORT ascii’ … ‘#+END_EXPORT’
;; c	‘#+BEGIN_CENTER’ … ‘#+END_CENTER’
;; C	‘#+BEGIN_COMMENT’ … ‘#+END_COMMENT’
;; e	‘#+BEGIN_EXAMPLE’ … ‘#+END_EXAMPLE’
;; E	‘#+BEGIN_EXPORT’ … ‘#+END_EXPORT’
;; h	‘#+BEGIN_EXPORT html’ … ‘#+END_EXPORT’
;; l	‘#+BEGIN_EXPORT latex’ … ‘#+END_EXPORT’
;; q	‘#+BEGIN_QUOTE’ … ‘#+END_QUOTE’
;; s	‘#+BEGIN_SRC’ … ‘#+END_SRC’
;; v	‘#+BEGIN_VERSE’ … ‘#+END_VERSE’

(define-key org-mode-map (kbd "<f5>") 'org-present)
(define-key org-present-mode-keymap (kbd "q") 'org-present-quit)
(define-key org-present-mode-keymap (kbd "<f5>") 'org-present-quit)

(provide 'facundo-org)

;;; facundo-org.el ends here
