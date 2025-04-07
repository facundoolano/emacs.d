;;; facundo-project.el --- project(ile) related configuration  -*- lexical-binding: t; -*-

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


(setq shell-file-name "/bin/zsh")

;; FIXME this should either not kill project buffers
;; or do it only if they are not already visible in another frame
(defun kill-project-frame ()
  "Delete current frame and kill all project buffers."
  (interactive)
  (if (> (length (frame-list)) 1)
      (progn
        (mapc 'kill-buffer (projectile-project-buffers))
        (delete-frame))
    (message "Attempted to kill the solely visible frame.")))

(defun facundo-switch-project-action ()
  "Show magit and the readme when opening a project."
  (interactive)
  (select-frame (make-frame))
  (let ((readme (concat (project-root (project-current)) "README.md")))
    (if (file-exists-p readme)
        (find-file readme)))
  (magit-project-status)
  (dired-sidebar-show-sidebar))

;; make switch project action open using the function above instead of prompting
(customize-set-variable 'project-switch-commands 'facundo-switch-project-action)

;;; navigate buffers
(defun next-project-buffer ()
  "Switch to the next user buffer within the current project."
  (interactive)
  (if-let ((proj (project-current)))
      (let ((proj-buffers (project-buffers proj))
            (start-buffer (current-buffer))
            (i 0))
        (next-buffer)
        (while (< i 20)
          (if (or
               (not (memq (current-buffer) proj-buffers))
               (not (buffer-file-name (current-buffer))))
              (progn
                (next-buffer)
                (setq i (1+ i))
                ;; Break if we've cycled through all buffers
                (when (eq (current-buffer) start-buffer)
                  (setq i 100)))
            (setq i 100))))
    (message "Not in a project")))


(defun previous-project-buffer ()
  "Switch to the previous user buffer within the current project."
  (interactive)
  (if-let ((proj (project-current)))
      (let ((proj-buffers (project-buffers proj))
            (start-buffer (current-buffer))
            (i 0))
        (previous-buffer)
        (while (< i 20)
          (if (or (not (memq (current-buffer) proj-buffers))
                  (not (buffer-file-name (current-buffer))))
              (progn
                (previous-buffer)
                (setq i (1+ i))
                ;; Break if we've cycled through all buffers
                (when (eq (current-buffer) start-buffer)
                  (setq i 100)))
            (setq i 100))))
    (message "Not in a project")))

;; FIXME rewrite
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

(defun project-find-file-other-window ()
  "Open FILENAME from a project in another window."
  ;; from https://github.com/midsbie/emacs-init
  (interactive)
  (unwind-protect
      (progn
        (advice-add 'find-file :override #'find-file-other-window)
        (project-find-file))
    (advice-remove 'find-file #'find-file-other-window)))

(defun projectile-discover-from-github (user-repo)
  "Clone a GitHub project and add it to Projectile projects.
USER-REPO should be a string in the format <username/reponame>."
  (interactive "sGitHub user/repo: ")
  (let* ((dev-dir "~/dev/")
         (full-path (concat dev-dir user-repo))
         (user-dir (file-name-directory full-path)))
    (unless (file-directory-p dev-dir)
      (make-directory dev-dir))
    (unless (file-directory-p user-dir)
      (make-directory user-dir))
    (unless (file-directory-p full-path)
      (shell-command
       (format "git clone git@github.com:%s.git %s" user-repo full-path)))
    ;; Discover and switch to the new project
    (projectile-discover-projects-in-directory user-dir)
    (projectile-switch-project-by-name full-path)))

(global-set-key (kbd "s-p") 'project-find-file)
(global-set-key (kbd "s-P") 'project-find-file-other-window)
(global-set-key (kbd "s-F") 'counsel-ag)

;; FIXME migrate
(global-set-key (kbd "s-w") 'kill-project-frame)
(global-set-key (kbd "s-W") 'delete-frame)
;; FIXME instead I want to open in new frame
(global-set-key (kbd "s-o") 'project-switch-project)

(global-set-key (kbd "C-<tab>") 'next-project-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-project-buffer)

(global-set-key (kbd "s-k") 'kill-this-and-next)
(global-set-key (kbd "s-K") 'kill-other-project-buffers)

(defun project-shell-command ()
  ""
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (shell-command-to-string (read-from-minibuffer "Shell command: "))))

(provide 'facundo-project)
;;; facundo-projectile.el ends here
