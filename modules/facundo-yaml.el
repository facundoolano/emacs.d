;;; Code:
(prelude-require-packages '(yaml-mode yaml-pro))

(require 'yaml-mode)
(require 'yaml-pro)

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.
(add-hook 'yaml-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook
          (lambda ()
            (visual-line-mode 0)))


(define-key yaml-mode-map (kbd "M-p") 'yaml-pro-ts-move-subtree-up)
(define-key yaml-mode-map (kbd "M-n") 'yaml-pro-ts-move-subtree-down)
(define-key yaml-mode-map (kbd "M-RET") 'yaml-pro-ts-meta-return)

(define-key yaml-mode-map (kbd "M-<left>") (lambda () (interactive)
                                             (let ((current-prefix-arg '(4))) ; Equivalent to C-u
                                               (yaml-pro-unindent-subtree))))
(define-key yaml-mode-map (kbd "M-<right>") (lambda () (interactive)
                                              (let ((current-prefix-arg '(4))) ; Equivalent to C-u
                                                (yaml-pro-indent-subtree))))

(provide 'facundo-yaml)
