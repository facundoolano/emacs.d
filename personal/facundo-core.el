;;; facundo-core.el --- my emacs config                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Facundo Olano

;; Author: Facundo Olano
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(prelude-require-packages '(spaceline monokai-theme hl-todo
                                      js2-highlight-vars toggle-quotes
                                      elixir-mode centered-window-mode
                                      cql-mode flycheck-pycheckers
                                      github-browse-file))

;;; Sublime like color theme
(disable-theme 'zenburn)
;; (load-theme 'sanityinc-tomorrow-day)
(load-theme 'monokai t)
(setq-default line-spacing 5)

(which-function-mode -1)

(setq powerline-default-separator 'utf-8)
(setq powerline-utf-8-separator-left        32
      powerline-utf-8-separator-right       32)

(require 'spaceline-config)
(spaceline-emacs-theme)
(spaceline-helm-mode)
(spaceline-toggle-projectile-root-on)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-minor-modes-off)

;;; show line numbers, but not on neotree
;; (setq linum-format 'dynamic)
(setq linum-format "%3d ")
(add-hook 'prog-mode-hook 'linum-mode)

(fringe-mode '(10 . 0))

;;; redo with cmd shift z
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;;; disable scrollbar
(scroll-bar-mode -1)

;;; remember window size
(desktop-save-mode 1)
(desktop-read)

;;; disable line wrapping
(set-default 'truncate-lines t)

;; TODO move these few to a "facundo-text.el"
;; wrap lines in text modes
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

(setq org-startup-folded nil)

(setq magit-no-confirm-default '(magit-branch-and-checkout))

;; highlights todo and fixme
(require 'hl-todo)
(global-hl-todo-mode t)
(setq hl-todo-activate-in-modes '(prog-mode emacs-lisp-mode))

;;; toggle comments
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun delete-line-or-region ()
  "Deletes (without copying) the current line or the lines encompassed by the current region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning) end (region-end))
          (save-excursion
            (setq beg (progn (goto-char beg) (line-beginning-position))
                  end (progn (goto-char end) (line-end-position)))))
      (setq beg (line-beginning-position) end (line-end-position)))
    (delete-region beg end)
    (delete-char 1)))

(defun my-replace-string ()
  "Move to beggining of buffer before replacing string."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'replace-string)))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))))

(defun new-empty-buffer-split ()
  "Open a new empty buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))))

(defadvice isearch-search (after isearch-no-fail activate)
  "Advice search to be wrapped by default."
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(setq mouse-wheel-scroll-amount '(0.05))
(setq mouse-wheel-progressive-speed nil)

(defun go-back ()
  (interactive)
  (universal-argument)
  (cua-set-mark
   `(4)))

(defun my/zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         10)))

(defun my/zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         10)))

(defun mark-and-search ()
  "Easy mark symbol current symbol and search for it in the current buffer "
  (interactive)
  (easy-mark 1)
  (cua-copy-region nil)
  (isearch-forward nil 1)
  (isearch-yank-kill))

(defun mark-and-grep ()
  "Easy mark symbol current symbol and search for it in the project files. "
  (interactive)
  (easy-mark 1)
  (cua-copy-region nil)
  (helm-projectile-ag))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-b") 'go-back)
(global-set-key (kbd "s-d") 'delete-line-or-region)
(global-set-key (kbd "M-s-f") 'my-replace-string)
(define-key prelude-mode-map (kbd "C-o") 'crux-smart-open-line)
(define-key prelude-mode-map (kbd "M-o") 'crux-smart-open-line-above)
(global-set-key (kbd "s-a") 'select-current-line)
(global-set-key (kbd "s-A") 'mark-whole-buffer)
(global-set-key (kbd "s-n") 'new-empty-buffer)
(global-set-key (kbd "s-N") 'new-empty-buffer-split)
(define-key emacs-lisp-mode-map (kbd "s-e") 'eval-region)
(global-set-key (kbd "C-M-f") 'forward-sexp)
(global-set-key (kbd "C-M-b") 'backward-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-s") 'sp-splice-sexp)
(global-set-key (kbd "s-V") 'cua-paste-pop) ; paste cycling through kill ring
(global-set-key (kbd "s-D") 'c-hungry-delete-forward) ; delete all following whitespaces
(global-set-key [(super backspace)] 'c-hungry-delete-backwards) ; delete all preceeding whitespaces
(global-set-key (kbd "C-'") 'toggle-quotes)
(global-set-key (kbd "C-S-s") 'mark-and-search)
(global-set-key (kbd "C-M-s") 'mark-and-grep)

(global-set-key (kbd "<f9>") 'centered-window-mode)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(setq comint-scroll-show-maximum-output nil)

(provide 'facundo-core)
;;; facundo-core.el ends here
