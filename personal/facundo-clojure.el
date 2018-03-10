;;; facundo-clojure.el --- clojure configuration     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano <facundo@madmobile>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
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

;; TODO rethink this, doesn't seem that useful anymore
(defun reload-and-eval-in-repl ()
  "Set the ns of the repl to the one in the current buffer, then eval the region of the whole buffer in the repl and switch to it."
  (interactive)
  (cider-repl-set-ns (cider-current-ns))
  (if (region-active-p)
      (cider-insert-region-in-repl (region-beginning) (region-end)))
  (cider-load-buffer-and-switch-to-repl-buffer)
  (cider-repl-return))

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
