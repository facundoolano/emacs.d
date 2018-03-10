;;; facundo-erlang.el --- erlang configuration       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano
;; () languages

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

(provide 'facundo-erlang)

(require 'prelude-erlang)

(require 'erlang-start)

;; default to erlang mode in .config files
(add-to-list 'auto-mode-alist '("\\.config\\’" . erlang-mode))

(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(setq erlang-man-root-dir "/usr/local/lib/erlang/man")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.9.1/emacs" load-path))
(setq erlang-indent-level 2)
(setq flycheck-erlang-include-path (list "../include/" "../../include/"))

(sp-local-pair 'erlang-mode "<<\"" "\">>")

(defun my-erlang-mode-hook ()
  (local-set-key (kbd "s-j") 'erlang-shell))

(defun my-erlang-shell-mode-hook ()
  (local-set-key (kbd "<up>") 'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input))

;; Some Erlang customization
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(add-hook 'erlang-shell-mode-hook 'my-erlang-shell-mode-hook)

;;; facundo-erlang.el ends here
