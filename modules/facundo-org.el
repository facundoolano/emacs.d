;;; facundo-org.el --- Emacs Prelude: org-mode configuration.
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; Code:

(prelude-require-packages '(org ox-gfm org-journal org-present ob-mermaid))

(require 'org)
(require 'org-present)
(require 'org-journal)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-edit-src-content-indentation nil)

(defun prelude-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))

(add-hook 'org-mode-hook 'prelude-org-mode-defaults)

;; syntax highlight latex (beamer) export
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-image-actual-width nil)


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

(setq org-export-with-sub-superscripts nil)

(setq org-present-text-scale 3)

(setq org-list-allow-alphabetical t)

;; insert new item after current item subitems on m ret
;; https://emacs.stackexchange.com/a/79066/14798
(customize-set-variable 'org-M-RET-may-split-line '((default . t) (item . nil)))

(set-face-background 'org-block "white")

;; setup presentation mode
(add-hook 'org-present-mode-hook
          (lambda ()
            (global-hl-line-mode -1)
            (org-present-big)
            (org-toggle-inline-images)
            (toggle-frame-fullscreen)
            (setq-local cwm-centered-window-width 140)
            (centered-window-mode)
            (flyspell-mode -1)
            (whitespace-mode -1)))


(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (global-hl-line-mode)
            (org-present-small)
            (toggle-frame-fullscreen)
            (org-toggle-inline-images)
            (centered-window-mode -1)
            (flyspell-mode +1)
            (whitespace-mode +1)))


(setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\"")
(setq org-journal-file-header "#+TODO: TODO STARTED | CANCELLED DONE\n\n")

(defun org-journal-open-or-create-current ()
  "Switch to the current journal file buffer without creating a new entry."
  (interactive)
  ;; keep a single open journal buffer
  (let ((current-buf (get-file-buffer (org-journal--get-entry-path))))
    (if current-buf
        (kill-buffer current-buf)))
  (setq current-prefix-arg '(4)) ; C-u to prevent entry creation
  (call-interactively 'org-journal-new-entry))

;; mermaid setup
(setq ob-mermaid-cli-path "~/.asdf/shims/mmdc")

;; auto display images for mermaid diagram convenience
;; TODO may not be a reasonable default for all projects
(setq org-confirm-babel-evaluate nil)
;; (eval-after-load 'org
;;   (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (emacs-lisp . t)))


(global-set-key (kbd "C-x j") 'org-journal-open-or-create-current)

(define-key org-mode-map (kbd "M-{") 'org-previous-visible-heading)
(define-key org-mode-map (kbd "M-}") 'org-next-visible-heading)
(define-key org-mode-map (kbd "M-p") 'org-metaup)
(define-key org-mode-map (kbd "M-n") 'org-metadown)
(define-key org-mode-map (kbd "M-<up>") 'move-text-up)
(define-key org-mode-map (kbd "M-<down>") 'move-text-down)

(define-key org-mode-map (kbd "<f5>") 'org-present)
(define-key org-present-mode-keymap (kbd "q") 'org-present-quit)
(define-key org-present-mode-keymap (kbd "<f5>") 'org-present-quit)

;; need a way to wrap region with emphasize words
;; https://emacs.stackexchange.com/questions/10029/org-mode-how-to-create-an-org-mode-markup-keybinding

(provide 'facundo-org)

;;; facundo-org.el ends here
