;;; facundo-emacs-lisp.el --- Emacs Prelude: Nice config for Elisp programming.
;;
;;; Commentary:

;; Nice config for Elisp Programming.

;;; Code:


;;; TAKE FROM prelude-emacs-lisp.el

(require 'facundo-lisp)
(require 'crux)

(use-package elisp-slime-nav)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun prelude-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (prelude-lisp-coding-defaults)
  (eldoc-mode +1))

(add-hook 'emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;;; CUSTOM STUFF

(define-key emacs-lisp-mode-map (kbd "s-e") 'eval-region)

(provide 'facundo-emacs-lisp)

;;; facundo-emacs-lisp.el ends here
