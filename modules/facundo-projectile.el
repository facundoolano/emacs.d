;;; facundo-projectile.el --- project(ile) related configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano;; <facundo@madmobile>
;; Keywords: projectile

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

(prelude-require-packages '(helm projectile nameframe-projectile))
(require 'projectile)
(require 'helm)

(setq projectile-globally-ignored-directories (append '("node_modules" "coverage") projectile-globally-ignored-directories))
(setq shell-file-name "/bin/sh")

(defun kill-project-frame ()
  "Delete current frame and kill all project buffers."
  (interactive)
  (mapc 'kill-buffer (projectile-project-buffers))
  (delete-frame))

;;; navigate buffers
(defun next-project-buffer ()
  "Switch to the next user buffer within the current project."
  (interactive)
  (let ((root (projectile-project-root)))
    (next-buffer)
    (let ((i 0))
      (while (< i 20)
        (if (or (not (user-buffer-q)) (not (projectile-project-buffer-p (current-buffer) root)))
            (progn (next-buffer)
                   (setq i (1+ i)))
          (progn (setq i 100)))))))

(defun previous-project-buffer ()
  "Switch to the previous user buffer within the current project."
  (interactive)
  (let ((root (projectile-project-root)))
    (previous-buffer)
    (let ((i 0))
      (while (< i 20)
        (if (or (not (user-buffer-q)) (not (projectile-project-buffer-p (current-buffer) root)))
            (progn (previous-buffer)
                   (setq i (1+ i)))
          (progn (setq i 100)))))))

(defun user-buffer-q (&optional buffer)
  "Return t if BUFFER is a user buffer, else nil.  BUFFER defaults to the current bufer."
  (interactive)
  (if (string-equal "*" (substring (buffer-name buffer) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t)))

(defun kill-this-and-next ()
  "Kill the current buffer and move to the next current project one if any, else find file in project."
  (interactive)
  (let ((next (car (-filter 'user-buffer-q (projectile-project-buffers-non-visible))))
        (pname (projectile-project-name)))
    (prin1 pname)
    (kill-buffer)
    (if next
        (switch-to-buffer next)
      (progn
        (projectile-switch-project-by-name pname) ;; FIXME this is not working
        (projectile-find-file)))))

(defun kill-other-project-buffers ()
  "Kill all user buffers from this project, except the current one."
  (interactive)
  (let ((project-buffers (-filter 'user-buffer-q (projectile-project-buffers-non-visible))))
    (mapcar 'kill-buffer project-buffers)))


;;; open project in new frame
(nameframe-projectile-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; copy pasted a modified version of the name frame function so it works in emacs for mac
;;; FIXME ask why this is needed maybe open a PR or fork
(defun nameframe-projectile--before-switch-project-hook ()
  "Hook to create/switch to a project's frame."
  (let* ((project-to-switch nameframe-projectile--project-to-switch)  ;; set by advise
         (name (file-name-nondirectory (directory-file-name project-to-switch)))
         (curr-frame (selected-frame))
         (frame-alist (nameframe-frame-alist))
         (frame (nameframe-get-frame name frame-alist)))
    (cond
     ;; project frame already exists
     ((and frame (not (equal frame curr-frame)))
      (select-frame-set-input-focus frame))
     ((not frame)
      (progn (nameframe-make-frame name) (select-frame-set-input-focus (nameframe-get-frame name)))))))

(define-key prelude-mode-map (kbd "s-p") nil)
(define-key prelude-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "s-p") 'helm-projectile-find-file)
(global-set-key (kbd "s-P") 'projectile-find-file-other-window)
(global-set-key (kbd "s-F") 'helm-projectile-ag)
(global-set-key (kbd "s-w") 'kill-project-frame)
(define-key prelude-mode-map (kbd "s-o") 'projectile-switch-project)

(global-set-key (kbd "C-<tab>") 'next-project-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-project-buffer)

(define-key prelude-mode-map (kbd "s-k") nil)
(global-set-key (kbd "s-k") 'kill-this-and-next)
(global-set-key (kbd "s-K") 'kill-other-project-buffers)


(provide 'facundo-projectile)
;;; facundo-projectile.el ends here
