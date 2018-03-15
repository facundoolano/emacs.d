;; mode line configuration

(prelude-require-package 'all-the-icons)
(require 'all-the-icons)
;; will also need to call `all-the-icons-install-fonts`

;; show fancy branch icon for current branch
(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1 :family ,(all-the-icons-octicon-family))
                 'display '(raise 0))
     (propertize (format " %s" branch)))))

(defvar mode-line-my-vc
  '(:propertize
    (:eval (when vc-mode
             (-custom-modeline-github-vc)))
    face mode-line-directory)
  "Formats the git branch.")

(defvar mode-line-major-mode
  '(:propertize
    (:eval mode-name))
  "Shows the current major mode.")

(defun sml/get-directory ()
  "Decide if we want directory shown. If so, return it."
  (file-relative-name
   (cond
    ;; In email attachments, buffer-file-name is non-nil, but
    ;; file-name-directory returns nil
    ((buffer-file-name) (or (file-name-directory (buffer-file-name)) ""))
    ((eq major-mode 'dired-mode)
     (replace-regexp-in-string "/[^/]*/$" "/" default-directory))
    ((and (symbolp major-mode)
          (member major-mode '(shell-mode eshell-mode term-mode)))
     default-directory)
    ;; In indirect buffers, buffer-file-name is nil. The correct value is
    ;; retrieved from the base buffer.
    ((buffer-base-buffer)
     (with-current-buffer (buffer-base-buffer) (sml/get-directory)))
    (t ""))
   (projectile-project-root)))

(defvar mode-line-directory
  '(:propertize
    (:eval (sml/get-directory)))
  "Shows the current major mode.")

(require 'flycheck)

(defvar mode-line-flycheck-status
  '(:propertize
    (:eval (flycheck-mode-line-status-text)))
  "Shows the current major mode.")

(setq-default mode-line-format
              (list
               " "
               mode-line-modified
               "  "
               ;; TODO show directory with same face as the filename?
               mode-line-directory
               mode-line-buffer-identification
               "   "
               mode-line-my-vc
               "   "
               mode-line-major-mode
               " "
               ;; make this prettier at some point
               mode-line-flycheck-status
               "   "
               ;; mode-line-modes

               ;; TODO right align
               ;; TODO don't show Top/Bot
               ;; TODO show 44:10 instead of (44,10)
               mode-line-position))

(provide 'facundo-modeline)
