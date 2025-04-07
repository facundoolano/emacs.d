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

(use-package all-the-icons-dired)
(use-package dired-sidebar
  :after all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :bind (("<f8>" . dired-sidebar-toggle-sidebar)
         :map dired-sidebar-mode-map
         ("SPC" . dired-sidebar-preview)
         ("R" . sidebar-rename)
         ("C" . sidebar-copy)))

(diff-hl-dired-mode 0)

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-l --group-directories-first")

(customize-set-variable 'dired-sidebar-use-custom-font t)
(customize-set-variable 'dired-sidebar-no-delete-other-windows t)
(setq dired-sidebar-face '((t (:height 120))))

(custom-set-faces
 '(dired-directory ((t (:foreground "DodgerBlue3" :height 130 :background unspecified :weight normal))))
 '(all-the-icons-dired-dir-face ((((background light)) :foreground "DodgerBlue3" :height 130 :background unspecified :weight normal)))
 '(dired-header ((t (:foreground "gray" :height 120  :background unspecified :weight normal)))))


(customize-set-variable 'dired-sidebar-width 26)

(defun dired-sidebar-preview ()
  "Open file at point in another buffer without leaving the dired sidebar."
  (interactive)
  (dired-sidebar-find-file)
  (dired-sidebar-jump-to-sidebar)
  (dired-next-line 1))

(defun sidebar-rename ()
  "Like `dired-do-rename' but with `default-directory' set to the one specified by listing header."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (call-interactively #'dired-do-rename)))

(defun sidebar-copy ()
  "Like `dired-do-copy' but with `default-directory' set to the one specified by listing header."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (call-interactively #'dired-do-copy)))

(provide 'facundo-dired)
;;; facundo-dired.el ends here
