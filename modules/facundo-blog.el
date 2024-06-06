;;; facundo-blog.el --- Commands for org/jekyll blog management.

;; Copyright (C) 2020  Facundo Olano

;; Author: Facundo Olano <facundo@Facundos-MacBook-Pro-2.local>

;;; Commentary:

;; Commands for org/jekyll blog management

;;; Code:

(require 'org)
(require 'ox-publish)
(require 'ox-md)
(require 'ox-gfm)

;;;###autoload
(define-derived-mode org-blog-mode org-mode "BLOG"
  "org-mode extension to write jekyll blog posts exported as html.")

;; default input method spanish
;; center window mode
(defun org-blog-setup ()
  "Setup the blog mode."
  (if (string= "es" (org-kw-language))
      (progn (ispell-change-dictionary "spanish")
          (set-input-method "spanish-prefix")))
  (flyspell-mode 1)
  (company-mode -1)
  (writeroom-mode 1)
  (hl-line-mode -1)
  (global-hl-line-mode -1)
  (text-scale-increase 1))

(add-hook 'org-blog-mode-hook 'org-blog-setup)

;; don't adapt local links. may want to make this setq-local on blog-mode setup if it bothers other use cases
(setq org-link-file-path-type 'relative)

;; not sure if this is necessary
(add-to-list 'org-export-backends 'gfm)

;; adapt the md publish function to be usable in the org-publish-project config
(defun org-md-publish-to-md (plist filename pub-dir)
 "Publish an org file to Markdown.
``''
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
 (org-publish-org-to 'gfm filename ".md" plist pub-dir))

(defun org-blog-insert-separator ()
  "Insert a *** separtor as an HTML export in the post body."
  (interactive)
  (insert "\n#+BEGIN_CENTER\n")
  (insert "\\lowast{} \\lowast{} \\lowast{}\n")
  (insert "#+END_CENTER\n\n"))

;; FIXME this is lousy but it's better than nothing
(defun org-kw-language ()
  (car (cdr (car (org-collect-keywords '("LANGUAGE"))))))

(define-key org-blog-mode-map (kbd "s-r") 'org-blog-publish-file)
(define-key org-blog-mode-map (kbd "s-R") 'org-blog-publish)

(provide 'facundo-blog)
;;; facundo-blog.el ends here
