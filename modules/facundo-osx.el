;;; facundo-osx.el --- configuration for homebrew emacs-mac  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano;; <facundo@madmobile>
;; Keywords: osx

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

;; Some OSX specific stuff.

;;; Code:


;; TAKEN FROM PRELUDE

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(use-package exec-path-from-shell :defer nil)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PATH")

;; proced-mode doesn't work on OS X so we use vkill instead
;;(autoload 'vkill "vkill" nil t)
;;(global-set-key (kbd "C-x p") 'vkill)

;; It's all in the Meta
(setq ns-function-modifier 'hyper)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; ;; CUSTOM STUFF

(cua-mode)

;; leave C-x/C-c alone, cut/copy are done with super
(setq cua-prefix-override-inhibit-delay nil)

;; TODO check if this is still needed after removing prelude defaults
(setq mac-option-key-is-meta nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(define-key cua--cua-keys-keymap (kbd "C-v") 'scroll-up-command)
(define-key cua--cua-keys-keymap (kbd "M-v") 'scroll-down-command)
(global-set-key (kbd "s-x") 'cua-cut-region)
(global-set-key (kbd "s-c") 'cua-copy-region)
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-l") 'goto-line)

(provide 'facundo-osx)
;;; facundo-osx.el ends here
