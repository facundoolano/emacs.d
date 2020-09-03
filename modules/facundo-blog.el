;;; facundo-blog.el --- Commands for org/jekyll blog management.

;; Copyright (C) 2020  Facundo Olano

;; Author: Facundo Olano <facundo@Facundos-MacBook-Pro-2.local>

;;; Commentary:

;; Commands for org/jekyll blog management

;;; Code:

(prelude-require-packages '(htmlize))

(require 'org)
(require 'cl)


(setq org-publish-project-alist
      '(("blog"
         ;; Path to org files.
         :base-directory "~/dev/facundoolano/facundoolano.github.io/org"
         :base-extension "org"

         ;; Path to Jekyll Posts
         :publishing-directory "~/dev/facundoolano/facundoolano.github.io"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t)))

(defun sluggify (string)
  "Turn a title STRING with Spanish characters into an URL friendly slug."
  (seq-reduce '(lambda (str chars)
                 (replace-regexp-in-string
                  (aref chars 0) (aref chars 1) (downcase str)))
              [["á" "a"] ["é" "e"] ["í" "i"] ["ó" "o"] ["ú" "u"] ["ñ" "n"] [" " "-"]]
              string))

;; TODO consider forcing the spanish input source
(defun org-blog-new-post (title)
  "Create a new org-file for a blog post as expected by Jekyll with TITLE."
  (interactive "MPost title: ")
  (let ((slug (sluggify title))
        (date (current-time)))
    (find-file (concat (projectile-project-root) "org/_posts/"
                       (format-time-string "%Y-%m-%d") "-" slug
                       ".org"))
    (insert "#+OPTIONS: toc:nil num:nil\n")
    (insert "#+LANGUAGE: es\n")
    (insert "#+BEGIN_EXPORT html\n")
    (insert "---\n")
    (insert "layout: post\n")
    (insert "title: \"") (insert title) (insert "\"\n")
    (insert "date: ") (insert (format-time-string "%Y-%m-%d %H:%M:%S")) (insert "\n")
    (insert "tags: []\n")
    (insert "---\n")
    (insert "#+END_EXPORT\n\n")))

(defun org-blog-reset-date ()
  "Prompt for a new blog post date and set it in the filename and the Jekyll \
header."
  (interactive)
  (if (not (s-contains? "org/_posts/" (pwd)))
      (error "Not visiting a blog buffer")
    (let* ((date (read-from-minibuffer "Post date: " (format-time-string "%Y-%m-%d")))
           (filename (buffer-name))
           (html-filename (concat "../../_posts/"
                                  (replace-regexp-in-string ".org$" ".html" filename)))
           (new-name (concat date (substring filename 10))))
      (rename-file filename new-name t)
      (set-visited-file-name new-name t t)
      (delete-file html-filename)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^date: .*$" nil t)
        (replace-match (concat "date: " date))))))

(defun org-blog-publish ()
  "Run org-publish on the blog project, without resetting the point in buffer."
  (interactive)
  (save-excursion
    (org-publish "blog")))

;; this is easier than overriding the translation
(customize-set-value 'org-html-footnotes-section
                     "<div id=\"footnotes\">
<!--h2 class=\"footnotes\">%s: </h2-->
<h3 class=\"footnotes\">Notas: </h3>
<div id=\"text-footnotes\">
%s
</div>
</div>")

(provide 'facundo-blog)
;;; facundo-blog.el ends here
