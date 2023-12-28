;;; facundo-python.el --- Emacs Prelude: python.el configuration.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

(prelude-require-packages '(lsp-mode lsp-pyright py-autopep8 python-isort))

(require 'electric)
(require 'python)
(require 'facundo-programming)
(require 'py-autopep8)
(require 'lsp-pyright)

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

(defun py-insert-pdb-trace ()
  "Insert instructions for the python debugger."
  (interactive)
  (python-indent-line)
  (insert "import pdb; pdb.set_trace()"))

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
  (lsp)
  (eldoc-mode 1)
  (python-isort-on-save-mode 1)
  (py-autopep8-mode)
  (setq-local lsp-pyright-python-executable-cmd "python3")
  (setq-local lsp-pyright-extra-paths (vector "venv"))
  (setq-local my-indentation-offset python-indent-offset)
  (setq-local forward-sexp-function nil)
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

(setq py-autopep8-options '("--max-line-length=100" "--select=E,F,W,C90 "))

(setq python-shell-interpreter "python3")

;; TODO new python package module

(prelude-require-package 'flycheck-pycheckers)

; (add-hook 'flycheck-mode-hook 'flycheck-pycheckers-setup)
(setq flycheck-pycheckers-checkers '(flake8 pylint))

(define-key python-mode-map (kbd "<backtab>") 'my-unindent)

;;  more reasonable behavior when indenting yanked blocks
(setq indent-region-function nil)

(provide 'facundo-python)

;;; facundo-python.el ends here
