;;; facundo-neotree.el --- neotree configuration     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano;;; <facundo@madmobile>
;; Keywords: neotree

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

(prelude-require-package 'neotree)

(require 'neotree)
(require 'projectile)

(setq neo-theme 'ascii)

(custom-set-faces
 '(neo-banner-face ((t (:foreground "gray"))))
 '(neo-root-dir-face ((t (:foreground "gray"))))
 '(neo-dir-link-face ((t (:foreground "DodgerBlue3"))))
 ;; '(neo-file-link-face ((t (:foreground "gray40"))))
 )

; yanked from spacemacs
(setq neo-window-width 28
      neo-create-file-auto-open t
      neo-show-updir-line nil
      neo-mode-line-type 'neotree
      neo-smart-open t
      neo-show-hidden-files nil
      neo-auto-indent-point t
      neo-vc-integration nil)

(setq neo-toggle-window-keep-p nil)

;; TODO reduce duplication
(defun neotree-project-sync ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name))
        (cw (selected-window)))
    (neotree-hide-if-other-project)
    (neotree-show)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))
    (select-window cw)))

(defun neotree-project-toggle ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-hide-if-other-project)
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(defun neotree-hide-if-other-project ()
  "If the neotree window is open but in a different project from the one visible, close it."
  (if (and (neo-global--window-exists-p)
           (not (eq (window-frame neo-global--window) (selected-frame))))
      (neotree-hide)))

(setq neo-confirm-change-root "Off")

(global-set-key (kbd "<f8>") 'neotree-project-toggle)

(defun my/neotree-hook (_unused)
  "Disable line line numbers in neotree."
  (linum-mode -1)
  (toggle-scroll-bar -1))

(add-hook 'neo-after-create-hook 'my/neotree-hook)

(provide 'facundo-neotree)
;;; facundo-neotree.el ends here
