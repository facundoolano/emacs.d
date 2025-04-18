;;; facundo-ivy.el --- ivy, counsel and swiper setup
;;
;;; Commentary:

;; Some configuration for ivy, counsel and swiper

;;; Code:

(use-package ivy)
(use-package counsel)
(use-package smex)
(use-package anzu)
(use-package async)
(use-package ivy-xref)

;; smex is used to sort commands in counsel-M-x by recency
(setq smex-save-file (expand-file-name "smex-items" savefile-dir))

(ivy-mode 1)
(global-anzu-mode +1)

;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
(setq ivy-use-virtual-buffers t)

;; show result counts
(setq ivy-count-format "(%d/%d) ")

;; no regexp by default
(setq ivy-initial-inputs-alist nil)

;; configure regexp engine.
(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t   . ivy--regex-ignore-order)))

;; wrap around search
(setq ivy-wrap t)

;; include ./ otherwise selecting the completed directory is weird
(setq ivy-extra-directories '("./"))

(setq xref-show-definitions-function #'ivy-xref-show-defs)
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

;; this could maybe be migrated to use swiper instead of isearch
(defun mark-and-search ()
  "Easy mark symbol current symbol and search for it in the current buffer."
  (interactive)
  (easy-mark 1)
  (cua-copy-region nil)
  (isearch-forward nil 1)
  (isearch-yank-kill))

(defun mark-and-search-backward ()
  "Easy mark symbol current symbol and search backward for it in the current buffer."
  (interactive)
  (easy-mark 1)
  (cua-copy-region nil)
  (isearch-backward nil 1)
  (isearch-yank-kill)
  (isearch-repeat-backward nil))

(defun mark-and-grep ()
  "Easy mark symbol current symbol and search for it in the project files."
  (interactive)
  (easy-mark 1)
  (if (use-region-p)
      (counsel-rg (buffer-substring-no-properties (region-beginning) (region-end)))
    (counsel-rg)))

(defun counsel-describe-function-or-variable ()
  "Display help about the currently selected ivy result.
Assumes the symbol is a function and tries with a variable describe-function fails."
  (interactive)
  (let ((inhibit-message t)
        (current-symbol (intern (ivy-state-current ivy-last))))
    (condition-case nil
        (describe-function current-symbol)
      ('error
       (describe-variable current-symbol)))))

(define-key counsel-describe-map (kbd "TAB") 'counsel-describe-function-or-variable)
(define-key ivy-occur-mode-map (kbd "n") 'next-error)
(define-key ivy-occur-mode-map (kbd "p") 'previous-error)
(define-key ivy-occur-grep-mode-map (kbd "n") 'next-error)
(define-key ivy-occur-grep-mode-map (kbd "p") 'previous-error)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; (global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "s-r") 'counsel-recentf)
(global-set-key (kbd "C-c f") 'counsel-recentf)

(global-set-key (kbd "C-S-s") 'mark-and-search)
(global-set-key (kbd "C-S-r") 'mark-and-search-backward)
(global-set-key (kbd "C-M-s") 'mark-and-grep)

(provide 'facundo-ivy)

;;; facundo-ivy.el ends here
