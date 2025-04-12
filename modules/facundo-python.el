;;; facundo-python.el --- Emacs Prelude: python.el configuration.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

(require 'electric)
(require 'python)
(require 'eglot)
(require 'facundo-programming)
(require 'apheleia)

(add-to-list 'eglot-server-programs
             '(python-mode . ("pyright-langserver" "--stdio")))
(add-to-list 'eglot-server-programs
             '(python-ts-mode . ("pyright-langserver" "--stdio")))

(setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
(setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))


(use-package pet)

(defun py-insert-pdb-trace ()
  "Insert instructions for the python debugger."
  (interactive)
  (python-indent-line)
  (insert "import pdb; pdb.set_trace()"))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

;; maybe related to this https://github.com/wyuenho/emacs-pet/issues/50
(defun python-eglot-refresh ()
  "Force a config reload. I find this is necessary for python projects after\
 an Emacs restart somewhat similar to shutdown eglot but faster and no prompt."
  (interactive)
  (let ((server (eglot-current-server)))
    (when (member '(python-mode . "python") (eglot--languages server))
      (message "Signaling config change to Python server: %s"
               (eglot-project-nickname server))
      (eglot-signal-didChangeConfiguration server))))

(defun prelude-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  ;; pet-mode magically detect venv in projects and injects it in pyright
  (pet-eglot-setup)
  (eglot-ensure)
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
