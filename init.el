;;; init.el --- Facundo's configuration entry point.
;;

;;; Commentary:

;; This file simply sets up the default load path and requires
;; various modules.

;; Always load newest byte code

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar modules-dir (expand-file-name  "modules" root-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar vendor-dir (expand-file-name "vendor" root-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar savefile-dir (expand-file-name "savefile" root-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)

;;; remember window layout
(setq desktop-save t)
(desktop-save-mode 1)

;; the core stuff
(require 'facundo-packages)
(require 'facundo-projectile)
(require 'facundo-editor)
(require 'facundo-ui)
(require 'facundo-global-keybindings)

(when (eq system-type 'darwin)
  (require 'facundo-osx))

(defun safe-require (module)
  "Load the given MODULE, catch and log any error, move on."
  (message (concat "loading " (symbol-name module)))
  (with-demoted-errors "THERE WAS AN ERROR: %s"
    (require module)))

;; Non core modules.
(safe-require 'facundo-ivy)
(safe-require 'facundo-indent)
(safe-require 'facundo-parens)
(safe-require 'facundo-git)
(safe-require 'facundo-modeline)
(safe-require 'facundo-dired)
(safe-require 'facundo-org)
(safe-require 'facundo-blog)
(safe-require 'facundo-dictionary)
(safe-require 'facundo-text)
(safe-require 'facundo-gpt)

;;; Programming languages support
(safe-require 'facundo-emacs-lisp)
(safe-require 'facundo-rust)
(safe-require 'facundo-js)
(safe-require 'facundo-treesitter)
(safe-require 'facundo-python)

;; send custom generated stuff to a separate file
(setq custom-file "~/.emacs.d/custom.el")

;;; init.el ends here
