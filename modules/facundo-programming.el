;;; facundo-programming.el --- Emacs Prelude: prog-mode configuration
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;;; Commentary:

;; Some basic prog-mode configuration and programming related utilities.

;;; Code:

(use-package smartparens)
(use-package eglot)
(use-package apheleia)

;; show single line eldoc in the minibuffer
;; if there is flymake error show that first
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq eldoc-echo-area-use-multiline-p nil)
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prelude-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  (electric-pair-mode -1)
  (smartparens-mode +1)
  (flymake-mode 1)
  (prelude-enable-whitespace)
  (prelude-local-comment-auto-fill))

(add-hook 'prog-mode-hook 'prelude-prog-mode-defaults)

(setq flymake-no-changes-timeout 1.0)
(setq xref-prompt-for-identifier nil)

;; format on save with apheleia
;; override python defaults
;; should also pick up gofmt and prettier without config
(apheleia-global-mode)

;;; CUSTOM STUFF

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun insert-todo ()
  "Add a TODO comment in the line above."
  (interactive)
  (crux-smart-open-line-above)
  (insert "TODO ")
  (comment-or-uncomment-region-or-line))

(defun insert-fixme ()
  "Add a FIXME comment in the line above."
  (interactive)
  (crux-smart-open-line-above)
  (insert "FIXME ")
  (comment-or-uncomment-region-or-line))

(defun my-eldoc-visual-line-mode (&rest _)
  (with-current-buffer eldoc--doc-buffer
    (setq-local visual-fill-column-center-text nil)
    (turn-on-visual-line-mode)))
(advice-add 'eldoc-doc-buffer :after #'my-eldoc-visual-line-mode)

;; Fixes html entities in eldoc buffer
;; https://emacs.stackexchange.com/a/82952/14798
(defvar rb--eldoc-html-patterns
  '(("&nbsp;" " ")
    ("&lt;" "<")
    ("&gt;" ">")
    ("&amp;" "&")
    ("&quot;" "\"")
    ("&apos;" "'"))
  "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

(defun rb--string-replace-all (patterns in-string)
  "Replace all cars from PATTERNS in IN-STRING with their pair."
  (mapc (lambda (pattern-pair)
          (setq in-string
                (string-replace (car pattern-pair) (cadr pattern-pair) in-string)))
        patterns)
  in-string)

(defun rb--eldoc-preprocess (orig-fun &rest args)
  "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
  (let ((doc (car args)))
    ;; The first argument is a list of (STRING :KEY VALUE ...) entries
    ;; we replace the text in each such string
    ;; see docstring of `eldoc-display-functions'
    (when (listp doc)
      (setq doc (mapcar
                 (lambda (doc) (cons
                                (rb--string-replace-all rb--eldoc-html-patterns (car doc))
                                (cdr doc)))
                 doc)))


    (apply orig-fun (cons doc (cdr args)))))

(advice-add 'eldoc-display-in-buffer :around #'rb--eldoc-preprocess)



(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-;") 'insert-todo)
(global-set-key (kbd "s-:") 'insert-fixme)

;; sexp commands tend to work well in non lisp langs too, so binding them globally
(global-set-key (kbd "C-M-f") 'forward-sexp)
(global-set-key (kbd "C-M-b") 'backward-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-s") 'sp-splice-sexp)

;; TODO would be good to have something similar for no eldoc / eglot managed langs eg elisp
(define-key eglot-mode-map (kbd "M-?") 'eldoc-doc-buffer)
(define-key prog-mode-map (kbd "M-.") 'xref-find-definitions)
(define-key prog-mode-map (kbd "C-M-.") 'xref-find-definitions-other-window)
(define-key prog-mode-map (kbd "M-,") 'pop-tag-mark)

(provide 'facundo-programming)
;;; facundo-programming.el ends here
