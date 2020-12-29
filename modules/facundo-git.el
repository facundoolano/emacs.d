(prelude-require-packages '(magit
                            forge
                            git-timemachine
                            gitconfig-mode
                            gitignore-mode
                            github-review
                            git-link))

(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(define-key magit-hunk-section-map (kbd "S-<return>") 'magit-diff-visit-file-other-window)

(add-to-list 'magit-dwim-selection '(magit-branch-and-checkout nil t))

(add-to-list 'magit-section-initial-visibility-alist '(stashes . hide))

(require 'git-link)

(setq git-link-open-in-browser t)

(defun facundo/git-link ()
  (interactive)
  (if current-prefix-arg
      (let ((git-link-use-commit t)
            (current-prefix-arg nil))
        (call-interactively 'git-link))
    (call-interactively 'git-link)))

(global-set-key (kbd "C-x l") 'facundo/git-link)

(provide 'facundo-git)
