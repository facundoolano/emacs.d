;;; facundo-python.el --- Emacs Prelude: python.el configuration.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

(require 'electric)
(require 'python)
(require 'facundo-programming)
(require 'apheleia)

(use-package lsp-pyright)

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
  (lsp-deferred)
  (setq-local my-indentation-offset python-indent-offset)
  (setq-local forward-sexp-function nil)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))

  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local))


(add-hook 'python-mode-hook 'prelude-python-mode-defaults)
(add-hook 'python-ts-mode-hook 'prelude-python-mode-defaults)

(setq python-shell-interpreter "python3")

(define-key python-mode-map (kbd "<backtab>") 'my-unindent)

;;  more reasonable behavior when indenting yanked blocks
(setq indent-region-function nil)

(provide 'facundo-python)

;;; facundo-python.el ends here
