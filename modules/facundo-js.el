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
(prelude-require-packages '(js2-mode json-mode add-node-modules-path mocha js2-highlight-vars))

(require 'facundo-programming)
(require 'js2-mode)
(require 'mocha)

;; taken from prelude
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (defun prelude-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq-local electric-layout-rules '((?\; . after)))
       (setq mode-name "JS2")
       (js2-imenu-extras-mode +1))

     (add-hook 'js2-mode-hook 'prelude-js-mode-defaults)))

;; custom stuff
(setq mocha-reporter "spec")
(setq mocha-options "--no-colors --recursive")

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

;; (define-key js2-mode-map (kbd "M-n i") 'npm-install)
;; (define-key js2-mode-map (kbd "M-n d") 'npm-new-dependency)
;; (define-key js2-mode-map (kbd "M-n p") 'npm-publish)
;; (define-key js2-mode-map (kbd "M-n M-t") 'mocha-test-project)
;; (define-key js2-mode-map (kbd "M-n t") 'mocha-test-file)
;; (define-key js2-mode-map (kbd "M-n T") 'mocha-test-at-point)
;; (define-key js2-mode-map (kbd "M-n v") 'npm-version)
(define-key js2-mode-map (kbd "RET") 'js2-line-break) ; auto closes comment blocks on enter
(define-key js2-mode-map (kbd "s-f") 'eslint-fix)
(define-key js2-mode-map (kbd "M-h") 'js2-highlight-vars-mode)

(require 'facundo-indent)

(setq js2-basic-offset my-indentation-offset)
(define-key js2-mode-map (kbd "<tab>") 'my-indent)
(define-key js2-mode-map (kbd "<backtab>") 'my-unindent)
(define-key js2-mode-map [(backspace)] 'backspace-whitespace-to-tab-stop)

(provide 'facundo-js)
;;; facundo-js.el ends here
