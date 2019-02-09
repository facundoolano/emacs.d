;;; facundo-text.el --- text edition configuration

(require 'livedown)

(prelude-require-package 'centered-window)
(global-set-key (kbd "<f9>") 'centered-window-mode)

;; wrap lines in text modes
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

(provide 'facundo-text)

;;; facundo-text.el ends here
