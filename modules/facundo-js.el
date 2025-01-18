;;; facundo-js.el --- node programming customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano;;; js customizations <facundo@madmobile>
;; Keywords: node.js javascript

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

;;

;;; Code:
(prelude-require-packages '(json-mode add-node-modules-path lsp-mode typescript-mode))

(require 'facundo-programming)
(require 'facundo-indent)
(require 'typescript-ts-mode)

;; taken from prelude
(add-to-list 'auto-mode-alist '("\\.js\\'"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"    . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'"    . tsx-ts-mode))

(add-hook 'tsx-ts-mode-hook #'lsp-deferred)
(add-hook 'typescript-ts-mode-hook #'lsp-deferred)

;; FIXME what do we need from this?
(eval-after-load 'js2-mode
  '(progn
     (defun prelude-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq-local electric-layout-rules '((?\; . after)))
       (setq mode-name "JS2")
       (js2-imenu-extras-mode +1))

     (add-hook 'js2-mode-hook 'prelude-js-mode-defaults)))

;; custom stuff
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

;; to avoid ugly output in npm commands
(add-to-list
 'comint-preoutput-filter-functions
 (lambda (output)
   (replace-regexp-in-string "\\[[0-9]+[GK]" "" output)))

(defun eslint-fix ()
  "Format the current file with ESLint."
  (interactive)
  (if (executable-find "eslint")
      (progn (call-process "eslint" nil "*ESLint Errors*" nil "--fix" buffer-file-name)
             (revert-buffer t t t))
    (message "ESLint not found.")))

(require 'facundo-indent)

(define-key typescript-ts-mode-map (kbd "<tab>") 'my-indent)
(define-key tsx-ts-mode-map (kbd "<tab>") 'my-indent)
(define-key typescript-ts-mode-map (kbd "<backtab>") 'my-unindent)
(define-key tsx-ts-mode-map (kbd "<backtab>") 'my-unindent)

(define-key js-mode-map [(backspace)] 'backspace-whitespace-to-tab-stop)

(provide 'facundo-js)
;;; facundo-js.el ends here
