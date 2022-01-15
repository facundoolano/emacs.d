;;; facundo-dired.el --- dired configuration     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano;;; <facundo@madmobile>
;; Keywords: dired

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

(prelude-require-packages '(dired-sidebar all-the-icons-dired))

(require 'ls-lisp)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(diff-hl-dired-mode 0)

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-lA --group-directories-first")

(customize-set-variable 'dired-sidebar-use-custom-font t)
(setq dired-sidebar-face '(:height 120))

(custom-set-faces
 '(dired-directory ((t (:foreground "DodgerBlue3" :height 130 :background nil :weight normal))))
 '(all-the-icons-dired-dir-face ((((background light)) :foreground "DodgerBlue3" :height 130 :background nil :weight normal)))
 '(dired-header ((t (:foreground "gray" :height 120  :background nil :weight normal))))
 )

(customize-set-variable 'dired-sidebar-width 26)

(global-set-key (kbd "<f8>") 'dired-sidebar-toggle-sidebar)

(provide 'facundo-dired)
;;; facundo-dired.el ends here
