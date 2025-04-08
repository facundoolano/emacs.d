;;; facundo-python.el --- Emacs Prelude: python.el configuration.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

(require 'electric)
(require 'python)
(require 'eglot)
(require 'facundo-programming)
(require 'apheleia)

(add-hook 'python-mode-hook #'eglot-ensure)

(add-to-list 'eglot-server-programs
             '(python-mode . ("pyright-langserver" "--stdio")))

(setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
(setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))


(defun py-insert-pdb-trace ()
  "Insert instructions for the python debugger."
  (interactive)
  (python-indent-line)
  (insert "import pdb; pdb.set_trace()"))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (eldoc-mode 1)
  ;; (setq-local lsp-pyright-python-executable-cmd "python3")
  ;; (setq-local lsp-pyright-extra-paths (vector "venv" ".venv"))
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
            #'electric-layout-post-self-insert-function nil 'local))


(add-hook 'python-mode-hook 'prelude-python-mode-defaults)

(setq flycheck-python-ruff-executable "ruff")

(setq python-shell-interpreter "python3")

;; TODO new python package module

(define-key python-mode-map (kbd "<backtab>") 'my-unindent)

;;  more reasonable behavior when indenting yanked blocks
(setq indent-region-function nil)

(provide 'facundo-python)

;;; facundo-python.el ends here
