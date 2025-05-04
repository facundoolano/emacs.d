;;; facundo-org.el --- Emacs Prelude: org-mode configuration.
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; Code:

(use-package org
  :bind (:map org-mode-map ("M-{" . org-previous-visible-heading)
              ("M-}" . org-next-visible-heading)
              ("M-p" . org-metaup)
              ("M-n" . org-metadown)
              ("M-<up>" . move-text-up)
              ("M-<down>" . move-text-down))
  :custom
  (org-log-done t)
  (org-edit-src-content-indentation nil)
  (org-startup-folded nil)
  (org-image-actual-width nil)
  (org-src-fontify-natively t) 
  (org-insert-heading-respect-content t)
  (org-M-RET-may-split-line '((default . t) (item . nil)))
  :hook
  (org-mode . (lambda ()
                (require 'org-tempo)
                (set-face-background 'org-block "white"))))

(use-package ox-gfm
  :custom
  (org-export-with-toc nil)
  (org-export-with-sub-superscripts nil)
  (org-list-allow-alphabetical t))

(use-package org-journal
  :bind
  ("C-x j" . org-journal-open-or-create-current)
  :custom
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\"")
  (org-journal-file-header "#+TODO: TODO STARTED | CANCELLED DONE\n\n")
  :config
  (defun org-journal-open-or-create-current ()
    "Switch to the current journal file buffer without creating a new entry."
    (interactive)
    ;; keep a single open journal buffer
    (let ((current-buf (get-file-buffer (org-journal--get-entry-path))))
      (if current-buf
          (kill-buffer current-buf)))
    (setq current-prefix-arg '(4)) ; C-u to prevent entry creation
    (call-interactively 'org-journal-new-entry)))

(use-package org-present
  :custom
  ;; hiding these specifically because I want bold table headings in org-present
  (org-hide-emphasis-markers t)
  ;; don't show line overflow in present mode
  (whitespace-global-modes '(not org-present-mode))
  (org-present-text-scale 3)
  
  :bind
  (:map org-mode-map
        ("<f5>" . org-present)
        :map org-present-mode-keymap
        ("q" . org-present-quit)
        ("<f5>" . org-present-quit))
  
  :hook
  ((org-present-mode . (lambda ()
                         (global-hl-line-mode -1)
                         (org-present-big)
                         (org-toggle-inline-images)
                         (toggle-frame-fullscreen)
                         (setq-local cwm-centered-window-width 140)
                         (centered-window-mode)
                         (flyspell-mode -1)
                         (whitespace-mode -1)))
   
   (org-present-mode-quit . (lambda ()
                              (global-hl-line-mode)
                              (org-present-small)
                              (toggle-frame-fullscreen)
                              (org-toggle-inline-images)
                              (centered-window-mode -1)
                              (flyspell-mode +1)
                              (whitespace-mode +1)))))


;; mermaid setup
(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "~/.asdf/shims/mmdc")
  (org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)
     (emacs-lisp . t))))
;; :hook
;; (org-babel-after-execute . org-redisplay-inline-images)


(provide 'facundo-org)

;;; facundo-org.el ends here
