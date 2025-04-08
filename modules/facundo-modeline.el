;; mode line configuration

(use-package all-the-icons)

(custom-set-faces
 '(mode-line ((t (:background "#335EA8" :box (:line-width 3 :color "#335EA8")))))
 '(mode-line-inactive ((t (:background "#9B9C97" :box (:line-width 3 :color "#9B9C97"))))))

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
             (-custom-modeline-github-vc))))
  "Formats the git branch.")

(defvar mode-line-major-mode
  '(:propertize
    (:eval mode-name))
  "Shows the current major mode.")

(defun sml/get-directory ()
  "Decide if we want directory shown. If so, return it."
  (when (project-current)
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
     (project-root (project-current)))))

(defvar mode-line-directory
  '(:propertize
    (:eval (sml/get-directory)))
  "Shows the current major mode.")

(defvar facundo/mode-line-position
  '(:propertize
    (:eval (format "[%%l/%d:%%c]" (line-number-at-pos (- (point-max) 1)))))
  "Shows the line, column and position in the current buffer.")


(setq-default mode-line-format
              (list
               "   "
               mode-line-directory
               mode-line-buffer-identification
               "  "
               facundo/mode-line-position
               "     "
               mode-line-my-vc
               "     "
               mode-line-major-mode
               ))
;; TODO right align


(provide 'facundo-modeline)
