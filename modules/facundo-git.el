(prelude-require-packages '(magit
                            git-timemachine
                            gitconfig-mode
                            gitignore-mode
                            github-browse-file))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; these are not working for some reason, prefix not allowed
;; (global-set-key (kbd "s-m m") 'magit-status)
;; (global-set-key (kbd "s-m l") 'magit-log)
;; (global-set-key (kbd "s-m f") 'magit-log-buffer-file)
;; (global-set-key (kbd "s-m b") 'magit-blame)

(setq magit-no-confirm-default '(magit-branch-and-checkout))

(require 'github-browse-file)

(defun github-save-url ()
  "Add the github url for the current line or region to the kill ring. "
  (interactive)
  (let ((github-browse-file-visit-url nil))
    (github-browse-file)))

(provide 'facundo-git)
