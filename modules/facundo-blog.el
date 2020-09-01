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
