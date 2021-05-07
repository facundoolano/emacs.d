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

(setq magit-bury-buffer-function 'magit-mode-quit-window)

(require 'git-link)

(setq git-link-open-in-browser t)
(setq git-link-default-branch "HEAD")

(defun facundo/git-link ()
  "Override the default git-link behavior to only show line number if there is
some region active and use a commit instead of the default branch when there's a prefix argument."
  (interactive)
  (let ((git-link-use-single-line-number mark-active)
        (git-link-use-commit current-prefix-arg)
        (current-prefix-arg nil))
    (call-interactively 'git-link)))

(global-set-key (kbd "C-x l") 'facundo/git-link)

(provide 'facundo-git)
