;;; facundo-python.el --- Emacs Prelude: python.el configuration.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

(prelude-require-packages '(anaconda-mode py-isort py-autopep8 python-mode))

(when (boundp 'company-backends)
  (prelude-require-package 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

(require 'electric)
(require 'facundo-programming)
(require 'py-isort)
(require 'py-autopep8)
(require 'python-mode)

;; Copy pasted from ruby-mode.el
(defun prelude-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun prelude-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun prelude-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun prelude-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (prelude-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (prelude-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (prelude-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (anaconda-mode 1)
  (eldoc-mode 1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'prelude-python-mode-set-encoding nil 'local))

(add-hook 'python-mode-hook 'prelude-python-mode-defaults)

(setq py-autopep8-options '("--max-line-length=200" "--select=E,F,W,C90 "))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(setq py-isort-options '("-l=100"))
(add-hook 'before-save-hook 'py-isort-before-save)

(setq python-shell-interpreter "/usr/bin/python")

(defun facundo-pythonic-activate ()
  "If there's a venv directory in the project root, activate it"
  (let ((venv (concat (projectile-project-root) "venv")))
    (if (file-directory-p venv)
        (pythonic-activate venv))))

;; TODO new python package module

(add-hook 'python-mode-hook 'facundo-pythonic-activate)

(prelude-require-package 'flycheck-pycheckers)

; (add-hook 'flycheck-mode-hook 'flycheck-pycheckers-setup)
(setq flycheck-pycheckers-checkers '(flake8 pylint))

(define-key python-mode-map (kbd "<backtab>") 'my-unindent)

(provide 'facundo-python)

;;; facundo-python.el ends here
