;;; facundo-blog.el --- Commands for org/jekyll blog management.

;; Copyright (C) 2020  Facundo Olano

;; Author: Facundo Olano <facundo@Facundos-MacBook-Pro-2.local>

;;; Commentary:

;; Commands for org+jorge blog management

;;; Code:

(require 'org)
(require 'ox-publish)
(require 'ox-md)
(require 'ox-gfm)
(require 'project)

;;;###autoload
(define-derived-mode org-blog-mode org-mode "BLOG"
  "org-mode extension to write blog posts in org-mode (using jorge).")

;; default input method spanish
;; center window mode
(defun org-blog-setup ()
  "Setup the blog mode."
  (if (string= "es" (org-kw-language))
      (progn (ispell-change-dictionary "spanish")
             (set-input-method "spanish-prefix")
             (setq-local org-footnote-section "Notas"))
    (setq-local org-footnote-section "Notes"))
  
  (flyspell-mode 1)
  (writeroom-mode 1)
  (hl-line-mode -1)
  (global-hl-line-mode -1)
  (text-scale-increase 1))

(add-hook 'org-blog-mode-hook 'org-blog-setup)

;; don't adapt local links. may want to make this setq-local on blog-mode setup if it bothers other use cases
(setq org-link-file-path-type 'relative)

(defun org-blog-new-post (title)
  "Create a new jorge post with the given TITLE and switch to it in blog mode."
  (interactive "MPost title: ")
  (let* ((project-root (project-root (project-current)))
         (default-directory project-root)
         (output (shell-command-to-string (concat "jorge post \"" title "\"")))
         (post-path (string-trim (string-remove-prefix "added " output))))
    (find-file (expand-file-name post-path project-root))
    (org-blog-mode)))

(defun org-blog-insert-separator ()
  "Insert a *** separtor as an HTML export in the post body."
  (interactive)
  (insert "\n#+BEGIN_CENTER\n")
  (insert "\\lowast{} \\lowast{} \\lowast{}\n")
  (insert "#+END_CENTER\n\n"))

;; FIXME this is lousy but it's better than nothing
(defun org-kw-language ()
  (car (cdr (car (org-collect-keywords '("LANGUAGE"))))))

(provide 'facundo-blog)
;;; facundo-blog.el ends here
