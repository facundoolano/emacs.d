;;; facundo-global-keybindings.el --- Emacs Prelude: some useful keybindings.

;;; Commentary:

;; Lots of useful keybindings.

;;; Code:

;;; TAKEN FROM prelude-global-keybindings.el

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Indentation help
(global-set-key (kbd "C-^") 'crux-top-join-line)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; replace zap-to-char functionality with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'crux-kill-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-=") 'er/expand-region)

;;; TAKEN FROM prelude-mode.el

(require 'imenu-anywhere)
(require 'crux)

;; TODO keep whats used, remove what's not or is overriden
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key [(shift return)] 'crux-smart-open-line)
(global-set-key (kbd "M-o") 'crux-smart-open-line)
(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [(meta up)]  'move-text-up)
(global-set-key [(meta down)]  'move-text-down)
(global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-repgion)
(global-set-key (kbd "C-M-z") 'crux-indent-defun)
(global-set-key (kbd "C-c u") 'crux-view-url)
(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c s") 'crux-swap-windows)
(global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
(global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
(global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
(global-set-key (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
(global-set-key (kbd "C-c I") 'crux-find-user-init-file)
(global-set-key (kbd "C-c S") 'crux-find-shell-init-file)
(global-set-key (kbd "C-c i") 'imenu-anywhere) ;; not sure what this does
;; extra prefix for projectile
(global-set-key (kbd "s-p") 'projectile-command-map)
(global-set-key (kbd "s-j") 'crux-top-join-line)
(global-set-key (kbd "s-k") 'crux-kill-whole-line)

(global-set-key (kbd "s-o") 'crux-smart-open-line-above)

;;; CUSTOM STUFF

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-b") 'go-back)
(global-set-key (kbd "s-d") 'delete-line-or-region)
(global-set-key (kbd "M-s-f") 'my-replace-string)
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "M-o") 'crux-smart-open-line-above)
(global-set-key (kbd "s-a") 'select-current-line)
(global-set-key (kbd "s-A") 'mark-whole-buffer)
(global-set-key (kbd "s-n") 'new-empty-buffer)
(global-set-key (kbd "s-N") 'new-empty-buffer-split)

(global-set-key (kbd "s-V") 'cua-paste-pop) ; paste cycling through kill ring
(global-set-key (kbd "C-S-s") 'mark-and-search)
(global-set-key (kbd "C-M-s") 'mark-and-grep)

(require 'toggle-quotes)
(global-set-key (kbd "C-'") 'toggle-quotes)

(load "cc-mode") ; needed for hungry-delete commands
(global-set-key (kbd "s-D") 'c-hungry-delete-forward) ; delete all following whitespaces
(global-set-key [(super backspace)] 'c-hungry-delete-backwards) ; delete all preceeding whitespaces

(provide 'facundo-global-keybindings)

;;; facundo-global-keybindings.el ends here
