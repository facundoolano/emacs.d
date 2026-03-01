;;; facundo-text.el --- text edition configuration

(require 'markdown-mode)
(require 'livedown)

(use-package centered-window)
(use-package visual-fill-column)
(use-package writeroom-mode)
(use-package reverse-im)
(use-package plantuml-mode)
(use-package markdown-toc)

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun text-hook ()
  (whitespace-mode -1)
  (setq sentence-end-double-space nil)
  (visual-line-mode))

;; wrap lines in text modes
(add-hook 'text-mode-hook 'text-hook)
(add-hook 'org-mode-hook 'text-hook)
(add-hook 'markdown-mode-hook 'text-hook)

(customize-set-variable 'writeroom-fullscreen-effect 'maximized)
(customize-set-variable 'writeroom-extra-line-spacing 1.15)

(customize-set-variable 'writeroom-width 85)

(defun highlight-visual-line ()
  "Range function to highlight the visual line instead of the physic one."
  (save-excursion
    (cons (progn (beginning-of-visual-line) (point))
          (progn (end-of-visual-line) (point)))))

(defun setup-writeroom ()
  "Setup conveniences for text writing."

  (setq hl-line-range-function 'highlight-visual-line)
  ;; FIXME this breaks blog mode?
  ;;  (text-mode)

  ;; make scale commands work with this mode
  (advice-add 'text-scale-increase :after
              #'visual-fill-column-adjust)
  (advice-add 'text-scale-decrease :after
              #'visual-fill-column-adjust)

  ;; prefer bottom modeline
  (customize-set-variable 'writeroom-mode-line-toggle-position 'mode-line-format)

  ;; removes weird whitespace highlighting
  (whitespace-mode -1))

(add-hook 'writeroom-mode-hook 'setup-writeroom)

;; (defun toggle-dictionary ()
;;   "Switch between english and spanish ispell dictionaries"
;;   (interactive)
;;   (if (string-equal ispell-current-dictionary "english")
;;       (ispell-change-dictionary "es")
;;     (ispell-change-dictionary "english"))
;;   (flyspell-buffer))

(global-set-key (kbd "<f9>") 'writeroom-mode)
;; (global-set-key (kbd "s-<f9>") 'toggle-dictionary)

;; some handy writing shortcuts
;; FIXME I had to comment this binding out of undo-tree to make it work
;; (global-set-key (kbd "M-_") (lambda () (interactive) (insert "---")))
(define-key text-mode-map (kbd "M-?") (lambda () (interactive) (insert "¿")))

;; use a plantuml executable for previewing of plantuml files
;; assumes plantuml executable installed and in path
(setq plantuml-default-exec-mode 'executable)
(setq plantuml-indent-level 2)

(defun markdown-meta-return ()
  "Insert new list item or heading depending on context, like org-meta-return."
  (interactive)
  (cond
   ;; If in a list, insert new list item
   ((markdown-list-item-at-point-p)
    (end-of-line)
    (call-interactively 'markdown-insert-list-item))
   ;; If on a heading, insert new heading at same level
   ((markdown-on-heading-p)
    (markdown-insert-header-respect-content))
   ;; Otherwise, just insert a newline
   (t
    (markdown-enter-key))))

(defun markdown-insert-header-respect-content ()
  "Insert heading after current section's content by inserting above and swapping."
  (interactive)
  (if (markdown-on-heading-p)
      (progn
        (beginning-of-line)
        (open-line 1)
        (markdown-insert-header-dwim)
        (markdown-move-down)
        (end-of-line))
    
    (markdown-insert-header-dwim)))

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("<M-right>" . markdown-demote)
              ("<M-left>" . markdown-promote)
              ("M-p" . markdown-move-up)
              ("M-n" . markdown-move-down)
              ("<M-up>" . move-text-up)
              ("<M-down>" . move-text-down)
              ("M-<return>" . markdown-meta-return))
  :custom
  (markdown-asymmetric-header t)
  (markdown-disable-tooltip-prompt t))



(provide 'facundo-text)

;;; facundo-text.el ends here
