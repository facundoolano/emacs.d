;;; facundo-js.el --- node programming customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano;;; js customizations <facundo@madmobile>
;; Keywords: node.js javascript

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
(require 'facundo-programming)
(require 'facundo-complete)

(use-package json-mode :mode "\\.json\\'")
(use-package json-mode :mode "\\.json\\'")

(use-package typescript-ts-mode
  :mode (("\\.js\\'"    . typescript-ts-mode)
         ("\\.ts\\'"    . typescript-ts-mode)
         ("\\.jsx\\'"    . tsx-ts-mode)
         ("\\.tsx\\'"    . tsx-ts-mode))

  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :bind (:map typescript-ts-mode-map
              ("<tab>" . my-indent)
              ("<backtab>" . my-unindent)
              :map tsx-ts-mode-map
              ("<tab>" . my-indent)
              ("<backtab>" . my-unindent)))

(provide 'facundo-js)
;;; facundo-js.el ends here
