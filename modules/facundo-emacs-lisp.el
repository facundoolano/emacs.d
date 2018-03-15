;;; facundo-emacs-lisp.el --- Emacs Prelude: Nice config for Elisp programming.
;;
;;; Commentary:

;; Nice config for Elisp Programming.

;;; Code:


;;; TAKE FROM prelude-emacs-lisp.el

(require 'facundo-lisp)
(require 'crux)

(prelude-require-packages '(elisp-slime-nav rainbow-mode))

(defun prelude-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p prelude-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun prelude-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun prelude-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun prelude-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'prelude-lisp-coding-defaults)
  (eldoc-mode +1)
  (prelude-recompile-elc-on-save)
  (rainbow-mode +1)
  (prelude-conditional-emacs-lisp-checker))

(add-hook 'emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; ielm is an interactive Emacs Lisp shell
(defun prelude-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'prelude-interactive-lisp-coding-defaults)
  (eldoc-mode +1))

(add-hook 'ielm-mode-hook 'prelude-ielm-mode-defaults)

(eval-after-load "ielm"
  '(progn
     (define-key ielm-map (kbd "M-(") (prelude-wrap-with "("))
     (define-key ielm-map (kbd "M-\"") (prelude-wrap-with "\""))))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;;; CUSTOM STUFF

(define-key emacs-lisp-mode-map (kbd "s-e") 'eval-region)

(provide 'facundo-emacs-lisp)

;;; facundo-emacs-lisp.el ends here
