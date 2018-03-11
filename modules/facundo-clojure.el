;;; facundo-clojure.el --- Emacs Prelude: Clojure programming configuration.
;;
;;; Commentary:

;; Some basic configuration for clojure-mode.


;;; Code:

;; TAKEN FROM prelude-clojure.el

(require 'facundo-lisp)
(prelude-require-packages '(clojure-mode cider))

(eval-after-load 'clojure-mode
  '(progn
     (defun prelude-clojure-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'prelude-lisp-coding-defaults))

     (add-hook 'clojure-mode-hook 'prelude-lisp-coding-defaults)))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)

     (add-hook 'cider-mode-hook 'eldoc-mode)

     (defun prelude-cider-repl-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'prelude-interactive-lisp-coding-defaults))

     (add-hook 'cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)))

;;; CUSTOM STUFF

(require 'cider)
(require 'flycheck-joker)

(setq cider-repl-scroll-on-output nil)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-file ".cider-repl-history")
(setq cider-prompt-for-symbol nil)
(setq clojure-align-forms-automatically t)
;; not so sure about this...
;; (add-to-list 'prelude-indent-sensitive-modes 'clojure-mode)
;; (add-to-list 'crux-indent-sensitive-modes 'clojure-mode)

(defun clojure-run-ns-tests ()
  "Refresh cider and run test for the current namespace."
  (interactive)
  (cider-refresh 'clear)
  (cider-test-run-ns-tests nil))

(defun clojure-run-project-tests ()
  "Refresh cider and run test for the entire project."
  (interactive)
  (cider-refresh 'clear)
  (cider-test-run-project-tests))

(defun cider-repl-repeat-command ()
  "Repeat the last command run in the repl. Useful to reload and run tests."
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-previous-input)
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))

;; TODO rethink this, doesn't seem that useful anymore
(defun reload-and-eval-in-repl ()
  "Set the ns of the repl to the one in the current buffer, then eval the region of the whole buffer in the repl and switch to it."
  (interactive)
  (cider-repl-set-ns (cider-current-ns))
  (if (region-active-p)
      (cider-insert-region-in-repl (region-beginning) (region-end)))
  (cider-load-buffer-and-switch-to-repl-buffer)
  (cider-repl-return))

(define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
(define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
(define-key cider-repl-mode-map (kbd "<S-return>") 'newline)
(define-key cider-repl-mode-map (kbd "<S-k>") 'kill-this-buffer)
(define-key clojure-mode-map (kbd "s-e") 'reload-and-eval-in-repl)
(define-key clojure-mode-map (kbd "s-j") 'cider-jack-in)
(define-key clojure-mode-map (kbd "s-r") 'cider-repl-repeat-command)

;; this bindings dont make much sense but are the same that for node tests
(define-key clojure-mode-map (kbd "M-n t") 'clojure-run-ns-tests)
(define-key clojure-mode-map (kbd "M-n M-t") 'clojure-run-project-tests)

(provide 'facundo-clojure)

;;; facundo-clojure.el ends here
