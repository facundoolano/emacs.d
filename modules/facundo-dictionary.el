(prelude-require-packages '(google-translate popup))

(require 'google-translate)
(require 'google-translate-smooth-ui)

(setq google-translate-translation-directions-alist
      '(("en" . "es") ("es" . "en")))

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "es")

(defun google-translate ()
  "Translate word at point if one is selected, otherwise query translate."
  (interactive)
  (if (use-region-p)
      (google-translate-at-point)
    (google-translate-query-translate)))

(defun google-reverse-translate ()
  "Translate word at point if one is selected, otherwise query translate."
  (interactive)
  (if (use-region-p)
      (google-translate-at-point-reverse)
    (google-translate-query-translate-reverse)))

(global-set-key (kbd "s-t") 'google-translate)
(global-set-key (kbd "s-T") 'google-reverse-translate)

;; wrap lines in the help buffer (same as used by translate results)
(add-hook 'help-mode-hook 'visual-line-mode)

(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

(provide 'facundo-dictionary)
