(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t) ;; Auto-install packages

(use-package elmacro)
(use-package persistent-scratch)
(use-package crux)
(use-package discover-my-major)
(use-package undo-tree)

(use-package gleam-ts-mode
  :mode "\\.gleam\\'"
  :after lsp-mode
  :hook (gleam-ts-mode . lsp-deferred)
  :init (when (and (fboundp 'treesit-language-available-p)
                   (not (treesit-language-available-p 'gleam)))
          (gleam-ts-install-grammar)))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . subword-mode)
         (go-mode . (lambda () (setq tab-width 4)))))

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

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cmake-mode
  :mode (("\\.cmake\\'" . cmake-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)))

(use-package css-mode :mode "\\.css\\'")
(use-package csv-mode :mode "\\.csv\\'")

(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode)
         ("\\.elixir\\'" . elixir-mode)))

(use-package erlang :mode "\\.erl\\'")
(use-package go-mode :mode "\\.go\\'")
(use-package less-css-mode :mode "\\.less\\'")
(use-package lua-mode :mode "\\.lua\\'")

(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)))

(use-package php-mode :mode "\\.php\\'")
(use-package protobuf-mode :mode "\\.proto\\'")

(use-package dockerfile-mode :mode "Dockerfile\\'")

(provide 'facundo-packages)
