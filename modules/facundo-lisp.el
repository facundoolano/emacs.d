;;; facundo-lisp.el --- Emacs Prelude: Configuration common to all lisp modes.
;;
;;; Commentary:

;; Configuration shared between all modes related to lisp-like languages.

;;; Code:

(require 'facundo-programming)
(prelude-require-packages '(rainbow-delimiters))

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (prelude-wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-\"") (prelude-wrap-with "\""))


;; a great lisp coding hook
(defun prelude-lisp-coding-defaults ()
  (parinfer-rust-mode)
  (rainbow-delimiters-mode +1))

;; interactive modes don't need whitespace checks
(defun prelude-interactive-lisp-coding-defaults ()
  (parinfer-rust-mode)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(provide 'facundo-lisp)

;;; facundo-lisp.el ends here
