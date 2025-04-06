;;; init.el --- Facundo's configuration entry point.
;;

;;; Commentary:

;; This file simply sets up the default load path and requires
;; various modules.

;; Always load newest byte code

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ;; Auto-install packages

;; FIXME review if these are needed
(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")

(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")

(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;; remember window layout
;; FIXME review
(setq desktop-save t)
(desktop-save-mode 1)

(defun safe-require (module)
  "Load the given MODULE, catch and log any error, move on."
  (message (concat "loading " (symbol-name module)))
  (with-demoted-errors "THERE WAS AN ERROR: %s"
    (require module)))

;; the core stuff
(require 'prelude-packages)

;; require some packages that don't belong in any specific module
;; TODO maybe merge prelude packages here
(prelude-require-packages '(elmacro
                            persistent-scratch
                            crux
                            discover-my-major
                            epl
                            undo-tree))

(require 'facundo-editor)
(require 'facundo-ui)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'facundo-osx))

(require 'facundo-global-keybindings)

;; allow to use emacsclient for shell edition
(server-start)

;; Non core modules.
(safe-require 'facundo-ivy)

(safe-require 'facundo-indent)
(safe-require 'facundo-parens)
(safe-require 'facundo-git)
(safe-require 'facundo-projectile)
(safe-require 'facundo-modeline)
(safe-require 'facundo-dired)
(safe-require 'facundo-org)
(safe-require 'facundo-yaml)
(safe-require 'facundo-blog)
(safe-require 'facundo-dictionary)
(safe-require 'facundo-spotify)
(safe-require 'facundo-text)
(safe-require 'facundo-gpt)

;;; Programming languages support
(safe-require 'facundo-emacs-lisp)
;; (safe-require 'facundo-clojure)
;; (safe-require 'facundo-erlang)
(safe-require 'facundo-rust)
(safe-require 'facundo-js)
(safe-require 'facundo-treesitter)
(safe-require 'facundo-python)
(safe-require 'facundo-go)
(safe-require 'facundo-gleam)
;; (safe-require 'facundo-ruby)

;; (safe-require 'prelude-elixir)
;; (safe-require 'prelude-c)
(safe-require 'prelude-web)
(safe-require 'prelude-xml)

;; send custom generated stuff to a separate file
(setq custom-file "~/.emacs.d/custom.el")

;;; init.el ends here
