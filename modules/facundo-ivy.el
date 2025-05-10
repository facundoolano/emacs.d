;;; facundo-ivy.el --- ivy, counsel and swiper setup
;;
;;; Commentary:

;; Some configuration for ivy, counsel and swiper

;;; Code:


(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-wrap t)
  (ivy-extra-directories '("./"))
  :bind
  ("C-x b" . ivy-switch-buffer)
  :config
  (ivy-mode 1)
  :bind (:map ivy-occur-mode-map
              ("n" . next-error)
              ("p" . previous-error)
              :map ivy-occur-grep-mode-map
              ("n" . next-error)
              ("p" . previous-error)))

(use-package prescient)

(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1))

(use-package anzu
  :config
  (global-anzu-mode +1))

(use-package ivy-xref
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("s-r" . counsel-recentf)
         ("C-c f" . counsel-recentf)
         ("C-S-s" . mark-and-search)
         ("C-S-r" . mark-and-search-backward)
         ("C-M-s" . mark-and-grep)
         :map counsel-describe-map
         ("TAB" . counsel-describe-function-or-variable)))

(provide 'facundo-ivy)

;;; facundo-ivy.el ends here
