;;; prelude-packages.el --- Emacs Prelude: default package selection.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Prelude.

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
(require 'package)

;; FIXME remove
(defun prelude-require-package (package)
  "Install PACKAGE unless already installed.")

;; FIXME remove
(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically.")

(use-package elmacro)
(use-package persistent-scratch)
(use-package crux)
(use-package discover-my-major)
(use-package undo-tree)

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cmake-mode
  :mode (("\\.cmake\\'" . cmake-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)))

(use-package css-mode :mode "\\.css\\'")
(use-package csv-mode :mode "\\.csv\\'")

(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode)
         ("\\.elixir\\'" . elixir-mode)))

(use-package erlang :mode "\\.erl\\'")
(use-package gleam-ts-mode :mode "\\.gleam\\'")
(use-package go-mode :mode "\\.go\\'")
(use-package less-css-mode :mode "\\.less\\'")
(use-package lua-mode :mode "\\.lua\\'")

(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)))

(use-package php-mode :mode "\\.php\\'")
(use-package protobuf-mode :mode "\\.proto\\'")
(use-package rustic :mode "\\.rs\\'")

(use-package dockerfile-mode :mode "Dockerfile\\'")


;; FIXME rename to facundo packages
(provide 'facundo-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; prelude-packages.el ends here
