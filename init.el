;;; init.el --- Facundo's configuration entry point.
;;

;;; Commentary:

;; This file simply sets up the default load path and requires
;; various modules.

;; Always load newest byte code

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-prefer-newer t)

;; TODO review if we need all of this
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

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; the core stuff
(require 'prelude-packages)
(require 'facundo-ui)
(require 'facundo-editor)
(require 'facundo-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'facundo-osx))

;; Non core modules.
;; TODO migrate prelude modules
(require 'prelude-ido)
(require 'prelude-helm)
(require 'prelude-helm-everywhere)

(require 'facundo-core) ;; TODO should be distributed to other files
(require 'facundo-indent)
(require 'facundo-parens)
(require 'facundo-projectile)
(require 'facundo-neotree)

;; send custom generated stuff to a separate file
(setq custom-file "~/.emacs.d/custom.el")

;;; init.el ends here
