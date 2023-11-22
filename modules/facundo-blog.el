;;; facundo-blog.el --- Commands for org/jekyll blog management.

;; Copyright (C) 2020  Facundo Olano

;; Author: Facundo Olano <facundo@Facundos-MacBook-Pro-2.local>

;;; Commentary:

;; Commands for org/jekyll blog management

;;; Code:

(prelude-require-packages '(htmlize))

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
  (ispell-change-dictionary "spanish")
  (set-input-method "spanish-prefix")
  (flyspell-mode 1)
  (company-mode -1)
  (writeroom-mode 1)
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
         :body-only t)

        ("site"
         ;; Path to org files.
         :base-directory "~/dev/facundoolano/olano.dev/org"
         :base-extension "org"

         ;; Path to Jekyll Posts
         :publishing-directory "~/dev/facundoolano/olano.dev"
         :recursive t
         :publishing-function org-md-publish-to-md
         :md-toplevel-hlevel 2
         :headline-levels 4
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
    (insert "#+END_EXPORT\n\n"))
  (org-blog-mode))

(defun org-blog-insert-separator ()
  "Insert a *** separtor as an HTML export in the post body."
  (interactive)
  (insert "\n#+BEGIN_CENTER\n")
  (insert "\\ast{} \\ast{} \\ast{}\n")
  (insert "#+END_CENTER\n\n"))

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
      (org-blog-mode)
      (org-blog-publish-file))))



(defun org-blog-publish ()
  "Run org-publish on the blog project, without resetting the point in buffer."
  (interactive)
  (save-excursion
    (org-publish-current-project)))

(defun org-blog-publish-file ()
  "Run org-publish but only for the current file, without resetting the point in buffer."
  (interactive)
  (save-excursion
    (org-publish-current-file)))

;; TODO add support for linking to another blog post
;; with org-insert-link
;; https://orgmode.org/manual/Adding-Hyperlink-Types.html


;; gfm doesn't expose a footnote customization variable, so I'm overriding its internal function
(defun org-gfm-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist
          (cl-loop for (n type raw) in fn-alist collect
                   (cons n (org-trim (org-export-data raw info))))))
    (when fn-alist
      (format
       "<section class=\"footnotes\" markdown=1>\n## %s\n%s</section>"
       "Notas"
       (format
        "\n%s\n"
        (mapconcat
         (lambda (fn)
           (let ((n (car fn)) (def (cdr fn)))
             (format
              "%s %s\n"
              (format
               (plist-get info :html-footnote-format)
               (org-html--anchor
                (format "fn.%d" n)
                n
                (format " class=\"footnum\" href=\"#fnr.%d\"" n)
                info))
              def)))
         fn-alist
         "\n"))))))

(define-key org-blog-mode-map (kbd "s-r") 'org-blog-publish-file)
(define-key org-blog-mode-map (kbd "s-R") 'org-blog-publish)

(provide 'facundo-blog)
;;; facundo-blog.el ends here
