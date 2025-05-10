(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         :map magit-hunk-section-map
         ("S-<return>" . magit-diff-visit-file-other-window))
  :init
  ;; don't show diff by default when committing, to reduce delay in message buffer
  ;; instead use C-c C-d to show diff only when necessary
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  :custom
  (magit-bury-buffer-function 'magit-mode-quit-window)
  :config
  (add-to-list 'magit-dwim-selection '(magit-branch-and-checkout nil t))
  (add-to-list 'magit-section-initial-visibility-alist '(stashes . hide)))


(use-package forge 
  :after magit
  :bind (:map magit-status-mode-map
              ("C-x l" . forge-copy-url-at-point-as-kill))
  :custom
  (forge-topic-list-limit '(5 . -1))
  :config
  (remove-hook 'magit-status-sections-hook 'forge-insert-issues))

(use-package git-timemachine)
(use-package git-modes)
(use-package github-review)

(defun facundo/git-link ()
  "Override the default git-link behavior to only show line number if there is
some region active and use a commit instead of the default branch when there's
a prefix argument."
  (interactive)
  (let ((git-link-use-single-line-number mark-active)
        (git-link-use-commit current-prefix-arg)
        (current-prefix-arg nil))
    (call-interactively 'git-link)))

(use-package git-link
  :bind (("C-x l" . facundo/git-link))
  :custom
  (git-link-open-in-browser t)
  (git-link-default-branch "HEAD"))

(provide 'facundo-git)
