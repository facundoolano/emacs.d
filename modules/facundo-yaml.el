;;; Code:

(use-package yaml-mode)

(use-package yaml-pro
  :after yaml-mode
  :hook (yaml-mode . (lambda ()
                       (yaml-pro-mode 1)
                       (subword-mode)
                       (visual-line-mode 0)))
  :bind (:map yaml-mode-map
              ("M-p" . yaml-pro-ts-move-subtree-up)
              ("M-n" . yaml-pro-ts-move-subtree-down)
              ("M-RET" . yaml-pro-ts-meta-return)
              ("M-<left>" . (lambda () (interactive)
                              (let ((current-prefix-arg '(4)))
                                (yaml-pro-unindent-subtree))))
              ("M-<right>" . (lambda () (interactive)
                               (let ((current-prefix-arg '(4)))
                                 (yaml-pro-indent-subtree))))))

(provide 'facundo-yaml)
