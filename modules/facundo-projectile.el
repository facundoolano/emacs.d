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

(prelude-require-packages '(projectile counsel-projectile))

(require 'projectile)
(require 'counsel-projectile)

(setq projectile-completion-system 'ivy)

(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
(projectile-mode t)

;; don't fail when not in projectile mode
(setq projectile-require-project-root nil)

(setq projectile-globally-ignored-directories (append '("node_modules" "coverage") projectile-globally-ignored-directories))
(setq shell-file-name "/bin/sh")

(defun kill-project-frame ()
  "Delete current frame and kill all project buffers."
  (interactive)
  (if (> (length (frame-list)) 1)
      (progn
        (mapc 'kill-buffer (projectile-project-buffers))
        (delete-frame))
    (message "Attempted to kill the solely visible frame.")))

(defun facundo-switch-project-action ()
  ;; (let ((readme (concat (projectile-project-root) "README.md")))
  ;;   (if (file-exists-p readme)
  ;;       (find-file readme)))
  (magit-status)
  (delete-other-windows)
  (neotree-project-sync))

(setq projectile-switch-project-action 'facundo-switch-project-action)

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
        (counsel-projectile-find-file)))))

(defun kill-other-project-buffers ()
  "Kill all user buffers from this project, except the current one."
  (interactive)
  (let ((project-buffers (-filter 'user-buffer-q (projectile-project-buffers-non-visible))))
    (mapcar 'kill-buffer project-buffers)))

;; Fix discover projects not working
;; https://github.com/bbatsov/projectile/issues/1165
(defun projectile-discover-projects-in-directory (directory)
  "Discover any projects in DIRECTORY and add them to the projectile cache.
This function is not recursive and only adds projects with roots
at the top level of DIRECTORY."
  (interactive
   (list (read-directory-name "Starting directory: ")))
  (let ((subdirs (directory-files directory t)))
    (mapcar
     (lambda (dir)
       (when (and (file-directory-p dir)
                  (not (member (file-name-nondirectory dir) '(".." "."))))
         (let ((default-directory dir)
               (projectile-cached-project-root dir))
           (when (projectile-project-p)
             (projectile-add-known-project (projectile-project-root))))))
     subdirs)))

;;; open project in new frame
(defun counsel-projectile-find-file-other-window ()
  "Open a file in the current project in a separate window."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Find file: ")
            (projectile-current-project-files)
            :matcher counsel-projectile-find-file-matcher
            :require-match t
            :sort t
            :action #'counsel-projectile-find-file-action-other-window
            :caller 'counsel-projectile-find-file))

(global-set-key (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "s-p") 'counsel-projectile-find-file)
(global-set-key (kbd "s-P") 'counsel-projectile-find-file-other-window)
(global-set-key (kbd "s-F") 'counsel-projectile-ag)
(global-set-key (kbd "s-w") 'kill-project-frame)
;; FIXME counsel-projectile not working here
(global-set-key (kbd "s-o") 'projectile-switch-project)

(global-set-key (kbd "C-<tab>") 'next-project-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-project-buffer)

(global-set-key (kbd "s-k") 'kill-this-and-next)
(global-set-key (kbd "s-K") 'kill-other-project-buffers)


(provide 'facundo-projectile)
;;; facundo-projectile.el ends here
