;;; facundo-ivy.el --- ivy, counsel and swiper setup
;;
;;; Commentary:

;; Some configuration for ivy, counsel and swiper

;;; Code:


(prelude-require-packages '(ivy counsel swiper smex))

(require 'ivy)
(require 'counsel)

;; smex is used to sort commands in counsel-M-x by recency
(require 'smex)
(setq smex-save-file (expand-file-name "smex-items" prelude-savefile-dir))

(ivy-mode 1)

;; ;;; CUSTOM STUFF
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

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

;; don't include ./ and ../ in file selection
(setq ivy-extra-directories nil)

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

(define-key counsel-describe-map (kbd "TAB") 'pcounsel-describe-function-or-variable)


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "s-r") 'counsel-recentf)
(global-set-key (kbd "C-c f") 'counsel-recentf)

(provide 'facundo-ivy)

;;; facundo-ivy.el ends here
