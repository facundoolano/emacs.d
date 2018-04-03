(prelude-require-package 'google-translate)

(require 'google-translate)
(require 'google-translate-smooth-ui)

(setq google-translate-translation-directions-alist
      '(("en" . "es") ("es" . "en") ))

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "es")

(global-set-key (kbd "s-t") 'google-translate-at-point)
(global-set-key (kbd "s-T") 'google-translate-at-point-reverse)

(provide 'facundo-dictionary)
