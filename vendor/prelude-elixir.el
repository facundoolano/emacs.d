;;; prelude-elixir.el --- Emacs Prelude: Elixir programming support.
;;
;; Copyright © 2014-2016 Samuel Tonini
;;
;; Author: Samuel Tonini <tonini.samuel@gmail.com>
;; Version: 1.0.0
;; Keywords: convenience elixir

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for Elixir development.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'facundo-programming)

(prelude-require-packages '(elixir-mode lsp-mode flycheck-credo))

;; needed to install elixir from source for elixir-ls to pick up stdlib definitions
;; asdf install elixir ref:v1.17.1
;; asdf global elixir ref:v1.17.1
;; https://mister11.dev/posts/fixing_go_to_definition_in_elixir_stdlib/
;; https://elixirforum.com/t/can-i-point-elixir-lsp-to-built-in-module-sources/41564

(add-to-list 'exec-path "~/dev/elixir/elixir-ls")

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'flycheck-mode-hook 'flycheck-credo-setup)

(add-hook 'elixir-mode-hook
          (lambda ()
            (lsp)
            (add-hook 'before-save-hook 'elixir-format nil t)))



(provide 'prelude-elixir)

;;; prelude-elixir.el ends here
