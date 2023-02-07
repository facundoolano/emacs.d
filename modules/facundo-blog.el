;;; facundo-blog.el --- Commands for org/jekyll blog management.

;; Copyright (C) 2020  Facundo Olano

;; Author: Facundo Olano <facundo@Facundos-MacBook-Pro-2.local>

;;; Commentary:

;; Commands for org/jekyll blog management

;;; Code:

(prelude-require-packages '(htmlize))

(require 'org)
(require 'ox-publish)

;;;###autoload
(define-derived-mode org-blog-mode org-mode "BLOG"
  "org-mode extension to write jekyll blog posts exported as html.")

;; default input method spanish
;; center window mode
(defun org-blog-setup ()
  "Setup the blog mode."
  (ispell-change-dictionary "spanish")
  (set-input-method "spanish-prefix")
  (flyspell-mode 1)
  (company-mode -1)
  (writeroom-mode 1)
  (text-scale-increase 1))

(add-hook 'org-blog-mode-hook 'org-blog-setup)

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
              [["á" "a"] ["é" "e"] ["í" "i"] ["ó" "o"] ["ú" "u"] ["ñ" "n"]
               [" " "-"] ["[^a-z0-9-]" ""]]
              string))

;; TODO consider forcing the spanish input method
;; (set-input-method "spanish-prefix")
;;  also default ispell dictionary?
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
    ;; TODO consider adding empty cover, thumbnail img attrs
    (insert "title: \"") (insert title) (insert "\"\n")
    (insert "date: ") (insert (format-time-string "%Y-%m-%d %H:%M:%S")) (insert "\n")
    (insert "tags: []\n")
    (insert "---\n")
    (insert "#+END_EXPORT\n\n")))

(defun org-blog-insert-separator ()
  "Insert a *** separtor as an HTML export in the post body."
  (interactive)
  (insert "\n#+BEGIN_CENTER\n")
  (insert "\\ast{} \\ast{} \\ast{}\n")
  (insert "#+END_CENTER\n\n"))

;; FIXME figure out some way (probably in jekyll config) to allow setting a future date and have the post still showing up
;; TODO add git add and commit
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
        (replace-match (concat "date: " date)))
      (org-blog-mode))))


(defun org-blog-publish ()
  "Run org-publish on the blog project, without resetting the point in buffer."
  (interactive)
  (save-excursion
    (org-publish "blog")))

(defun org-blog-publish-file ()
  "Run org-publish but only for the current file, without resetting the point in buffer."
  (interactive)
  (save-excursion
    (org-publish-current-file)))

;; this is easier than overriding the translation
(customize-set-value 'org-html-footnotes-section
                     "<div id=\"footnotes\">
<!--h2 class=\"footnotes\">%s: </h2-->
<h3 class=\"footnotes\">Notas: </h3>
<div id=\"text-footnotes\">
%s
</div>
</div>")

(define-key org-blog-mode-map (kbd "s-r") 'org-blog-publish-file)
(define-key org-blog-mode-map (kbd "s-R") 'org-blog-publish)

(provide 'facundo-blog)
;;; facundo-blog.el ends here
